#include "Board.h"

#include <stdlib.h>

#include <algorithm>
#include <map>
#include <utility>

#include <glog/logging.h>

using namespace std;

std::ostream& operator<<(std::ostream& os, const Color& c) {
  switch (c) {
    case Color::BLACK:   return (os << "BLACK");
    case Color::WHITE:   return (os << "WHITE");
    case Color::UNKNOWN: return (os << "UNKNOWN");
  }
}

Color OppositeColor(Color c) {
  switch (c) {
    case Color::BLACK:   return Color::WHITE;
    case Color::WHITE:   return Color::BLACK;
    case Color::UNKNOWN: LOG(FATAL) << "OppositeColor(UNKNOWN) is undefined.";
  }
}

// *****************************************************************************
// Indexer.

void Indexer::GetPosition(Index i, int* x, int* y) const {
  *y = i % width_;
  *x = i / width_;
}

vector<Index> Indexer::GetNeighbors(Index i) const {
  int x, y;
  GetPosition(i, &x, &y);
  vector<Index> result;
  for (const auto& position : GetNeighbors(x, y)) {
    result.push_back(GetIndex(position.first, position.second));
  }
  return result;
}

vector<pair<int, int>> Indexer::GetNeighbors(int x, int y) const {
  vector<pair<int, int>> result;
  if (x > 0)
    result.push_back({x - 1, y    });
  if (y > 0)
    result.push_back({x    , y - 1});
  if (x < width_ - 1)
    result.push_back({x + 1, y    });
  if (y < width_ - 1)
    result.push_back({x    , y + 1});
  return result;
}

int Indexer::ManhattanDistance(Index i, Index j) const {
  int xi, yi, xj, yj;
  GetPosition(i, &xi, &yi);
  GetPosition(j, &xj, &yj);
  return abs(xi - xj) + abs(yi - yj);
}

string Indexer::DebugIndex(Index i) const {
  int x, y;
  GetPosition(i, &x, &y);
  ostringstream result;
  result << "(" << x << ", " << y << ")";
  return result.str();
}

std::string Indexer::DebugMove(const Move& move) const {
  ostringstream result;
  result << "(" << (move.index > 0 ? DebugIndex(move.index) : "-1")
         << ", " << move.color << ")";
  return result.str();
}

// *****************************************************************************
// IslandSet.

IslandSet::IslandSet(
    int width,  const map<pair<int, int>, int>& pos_to_numbers)
    : indexer_(width),
      white_parent_(width * width, -1),
      num_white_children_(width * width, 0),
      numbers_(width * width, 0) {
  for (const auto& pos_to_number : pos_to_numbers) {
    int number = pos_to_number.second;
    CHECK_NE(0, number);

    const pair<int, int>& position = pos_to_number.first;
    Index i = indexer_.GetIndex(position.first, position.second);

    numbers_[i] = pos_to_number.second;
    CHECK(SetWhite(i));
  }
}

bool IslandSet::SetWhite(Index i) {
  CHECK_EQ(-1, white_parent_[i]) << " " << indexer_.DebugIndex(i);
  CHECK_EQ(0, num_white_children_[i]) << " " << indexer_.DebugIndex(i);

  UndoEdit undo;
  undo.newly_white = i;
  undo.white_parent_index = i;
  undo.white_parent_previous_value = -1;
  undo.num_white_children_index = i;
  undo.num_white_children_previous_value = 0;
  last_edits_.push(undo);

  white_parent_[i] = i;
  num_white_children_[i] = 1;

  for (Index neighbor : indexer_.GetNeighbors(i)) {
    if (white_parent_[neighbor] != -1) {
      // The neighbor is white.
      if (!MergeWhites(i, neighbor))
        return false;
    }
  }
  return true;
}

bool IslandSet::MergeWhites(Index i, Index j) {
  UndoEdit undo;
  undo.newly_white = i;

  Index i_repr = GetWhiteRepresentative(i);
  Index j_repr = GetWhiteRepresentative(j);
  if (i_repr == j_repr) {
    // The two areas are merged already, we simply do nothing.
    return true;
  }

  if (numbers_[i_repr] > 0 && numbers_[j_repr] > 0)
    return false;
  if (numbers_[i_repr] > 0) {
    // For simplicity we chose that j is the cell who is (maybe) attached to a
    // number already.
    swap(i_repr, j_repr);
  }

  // We merge i into j. Since j might be attached to a number, that ensures that
  // this number remains its own parent.

  // First we save the current state to be able to undo.
  undo.white_parent_index = i_repr;
  undo.white_parent_previous_value = white_parent_[i_repr];
  undo.num_white_children_index = j_repr;
  undo.num_white_children_previous_value = num_white_children_[j_repr];
  last_edits_.push(undo);

  white_parent_[i_repr] = j_repr;
  num_white_children_[j_repr] += num_white_children_[i_repr];

  // If j is tied to a island, we check that it has not become too big.
  if (numbers_[j_repr] > 0 && num_white_children_[j_repr] > numbers_[j_repr])
    return false;
  return true;
}

Index IslandSet::GetWhiteRepresentative(Index i) const {
  Index parent = white_parent_[i];
  while (parent != i) {
    i = parent;
    parent = white_parent_[i];
  }
  return parent;
}

void IslandSet::Undo(Index i) {
  CHECK(!last_edits_.empty());
  do {
    const UndoEdit& undo = last_edits_.top();
    CHECK_EQ(i, undo.newly_white);

    white_parent_[undo.white_parent_index] = undo.white_parent_previous_value;
    num_white_children_[undo.num_white_children_index] =
      undo.num_white_children_previous_value;
    last_edits_.pop();
  } while (!last_edits_.empty() && last_edits_.top().newly_white == i);
}

// *****************************************************************************
// Board.

Board::Board(int width,
             const map<pair<int, int>, int>& pos_to_numbers)
    : width_(width),
      num_cells_(width_ * width_),
      indexer_(width_),
      cells_(num_cells_, Color::UNKNOWN),
      islands_(width, pos_to_numbers) {
  CHECK_GT(width_, 0);
  for (const auto& pos_to_number : pos_to_numbers) {
    const pair<int, int>& position = pos_to_number.first;
    cells_[indexer_.GetIndex(position.first, position.second)] = Color::WHITE;
  }
}

bool Board::ApplyMove(const Move& move) {
  CHECK_EQ(Color::UNKNOWN, cells_[move.index]);

  cells_[move.index] = move.color;

  switch (move.color) {
    case Color::BLACK:
      if (IsInBlackSquare(move.index) || IsSomeIslandTooSmall(move.index))
        return false;
      break;
    case Color::WHITE:
      if (!islands_.SetWhite(move.index))
        return false;
      break;
    default:
      LOG(FATAL) << "Invalid move " << move.index << " --> " << move.color;
  }
  return !AreBlackDisconnected();
}

void Board::ApplyMoveOrDie(const Move& move) {
  if (move.index == 25 && move.color == Color::WHITE) { // FIXME
    CHECK(true);
  }
  CHECK(ApplyMove(move))
      << "\n" << ToString(true) << "Move: " << indexer_.DebugMove(move);
}

bool Board::IsInBlackSquare(Index black) const {
  int x, y;
  indexer_.GetPosition(black, &x, &y);

  for (int xx : {x - 1, x + 1}) {
    if (xx < 0 || xx >= width_ ||
        cells_[indexer_.GetIndex(xx, y)] != Color::BLACK)
      continue;
    if (y > 0 && cells_[indexer_.GetIndex(x, y - 1)] == Color::BLACK &&
        cells_[indexer_.GetIndex(xx, y - 1)] == Color::BLACK)
      return true;
    if (y < width_ - 1 && cells_[indexer_.GetIndex(x, y + 1)] == Color::BLACK &&
        cells_[indexer_.GetIndex(xx, y + 1)] == Color::BLACK)
      return true;
  }
  return false;
}

bool Board::IsSomeIslandTooSmall(Index last_black) const {
  for (Index i = 0; i < num_cells_; ++i) {
    int number = islands_.GetNumber(i);
    if (number == 0 || islands_.GetIslandCurrentSize(i) == number) {
      // We skip the cell early if it already has the correct number of white
      // neighbors.
      continue;
    }
    if (indexer_.ManhattanDistance(last_black, i) >= number) {
      // The cell that was marked black is too far to affect this island.
      continue;
    }

    vector<bool> reachables(num_cells_, false);
    int num_reachable_cells =
      ReachableCellsNotColoredAtMost(i, Color::BLACK, number, &reachables);
    if (num_reachable_cells < number)
      return true;
  }
  return false;
}

bool Board::AreBlackDisconnected() const {
  Index black_index = -1;
  for (Index i = 0; i < num_cells_; ++i) {
    if (cells_[i] == Color::BLACK) {
      black_index = i;
      break;
    }
  }
  if (black_index == -1)
    return false;

  vector<bool> reachable(num_cells_, false);
  ReachableCellsNotColored(black_index, Color::WHITE, &reachable);

  for (Index i = 0; i < num_cells_; ++i) {
    if (!reachable[i] && cells_[i] == Color::BLACK)
      return true;
  }
  return false;
}

int Board::ReachableCellsNotColored(
    Index origin, Color barrier, vector<bool>* reachable) const {
  return ReachableCellsNotColoredAtMost(origin, barrier, num_cells_, reachable);
}

// Returns the number of cells newly marked as reachable.
int Board::MarkAndPropagate(Index origin,
                            Color barrier,
                            int max_num_cells,
                            vector<bool>* reachable) const {
  CHECK_NE(cells_[origin], barrier);
  CHECK(!(*reachable)[origin]);

  (*reachable)[origin] = true;
  --max_num_cells;
  int newly_reachable_cells = 1;

  for (Index neighbor : indexer_.GetNeighbors(origin)) {
    if (max_num_cells == 0)
      break;
    if (cells_[neighbor] != barrier && !(*reachable)[neighbor]) {
       int num_reachable =
         MarkAndPropagate(neighbor, barrier, max_num_cells, reachable);
       newly_reachable_cells += num_reachable;
       max_num_cells -= num_reachable;
    }
  }
  return newly_reachable_cells;
}

int Board::ReachableCellsNotColoredAtMost(
    Index origin,
    Color barrier,
    int max_num_cells,
    vector<bool>* reachable) const {
  return MarkAndPropagate(origin, barrier, max_num_cells, reachable);
}

void Board::UndoMove(const Move& move) {
  CHECK_NE(Color::UNKNOWN, move.color);
  CHECK_EQ(move.color, cells_[move.index]);

  cells_[move.index] = Color::UNKNOWN;
  if (move.color == Color::WHITE)
    islands_.Undo(move.index);
}

bool Board::HasMoveNoSolution(const Move& move, int num_moves_horizon) const {
  CHECK_GE(num_moves_horizon, 1);
  Board* that = const_cast<Board*>(this);
  bool legal_move = that->ApplyMove(move);
  bool result = !legal_move || HasNoSolution(move, num_moves_horizon - 1);
  that->UndoMove(move);
  return result;
}

/*
struct DistanceToLessThan {
  DistanceToLessThan(Indexer indexer, int origin)
      : indexer_(indexer), origin_(origin) {}
  bool operator()(Index i, Index j) const {
    return indexer_.ManhattanDistance(i, origin_) <
           indexer_.ManhattanDistance(j, origin_);
  }

 private:
  const Indexer indexer_;
  const Index origin_;
};
*/

bool Board::HasNoSolution(
    const Move& previous_move, int num_moves_horizon) const {
  if (num_moves_horizon == 0)
    return false;

  // vector<Index> next_moves;
  // next_moves.reserve(num_cells_);
  // for (Index i = 0; i < num_cells_; ++i) {
    // if (cells_[i] == Color::UNKNOWN) {
      // next_moves.push_back(i);
    // }
  // }
  // sort(next_moves.begin(), next_moves.end(),
       // DistanceToLessThan(indexer_, previous_move.index));
  // next_moves.resize(8);  // FIXME

  for (Index i : indexer_.GetNeighbors(previous_move.index)) {
    if (cells_[i] != Color::UNKNOWN) {
      continue;
    }

    if (HasMoveNoSolution({i, Color::BLACK}, num_moves_horizon) &&
        HasMoveNoSolution({i, Color::WHITE}, num_moves_horizon)) {
      return true;
    }
  }
  return false;
}

void Board::Progress(stack<Move>* applied_moves, int max_lookahead) {
  for (int lookahead = 1; lookahead <= max_lookahead; ++lookahead) {
    bool made_progress;
    do {
      made_progress = false;
      for (Index i = 0; i < num_cells_; ++i) {
        if (cells_[i] != Color::UNKNOWN)
          continue;

        Move black_move = {i, Color::BLACK};
        Move white_move = {i, Color::WHITE};
        if (HasMoveNoSolution(black_move, lookahead)) {
          ApplyMoveOrDie(white_move);
          applied_moves->push(white_move);
          made_progress = true;
        } else if (HasMoveNoSolution(white_move, lookahead)) {
          ApplyMoveOrDie(black_move);
          applied_moves->push(black_move);
          made_progress = true;
        }
      }
    } while (made_progress);
  }
}

bool Board::Solve() {
  cout << "Preprocessing..." << endl;
  ProgressTrivialBlacks();

  stack<Move> applied_moves;
  Progress(&applied_moves, 8);

  cout << ToString() << endl;

  cout << "Backtracking..." << endl;
  return Backtrack();
}

bool Board::Backtrack() {
  const Move move = GetNextGuessMove();
  if (move.index == -1) {
    // There is no unknown cells: we have found a solution.
    return true;
  }

  if (BacktrackWithMove(move))
    return true;

  const Move other_move = {move.index, OppositeColor(move.color)};
  return BacktrackWithMove(other_move);
}

bool Board::BacktrackWithMove(const Move& move) {
  if (ApplyMove(move) && Backtrack()) {
    return true;
  } else {
    UndoMove(move);
    return false;
  }
}

void Board::ProgressTrivialBlacks() {
  for (Index i = 0; i < num_cells_; ++i) {
    if (cells_[i] != Color::UNKNOWN)
      continue;
    bool reachable = false;
    for (Index j = 0; j < num_cells_; ++j) {
      const int number = islands_.GetNumber(j);
      if (number < 0)
        continue;
      if (indexer_.ManhattanDistance(j, i) < number) {
        reachable = true;
        break;
      }
    }
    if (!reachable) {
      ApplyMoveOrDie({i, Color::BLACK});
    }
  }
}

Move Board::GetNextGuessMove() const {
  int max_num_known_neighbors = -1;
  int bestIndex = -1;
  for (Index i = 0; i < num_cells_; ++i) {
    if (cells_[i] != Color::UNKNOWN)
      continue;

    int num_known_neighbors = 0;
    for (Index neighbor : indexer_.GetNeighbors(i)) {
      if (cells_[neighbor] != Color::UNKNOWN)
        ++num_known_neighbors;

      // Numbers count twice as more as known cells.
      if (islands_.GetNumber(neighbor) > 0)
        ++num_known_neighbors;
    }

    if (num_known_neighbors == 4) {
      return {i, Color::BLACK};
    }
    if (num_known_neighbors > max_num_known_neighbors) {
      max_num_known_neighbors = num_known_neighbors;
      bestIndex = i;
    }
  }
  return {bestIndex, Color::BLACK};
}

namespace {
// Returns the number of dashes skipped.
int SkipLineSeparator(istream* stream) {
  int num_dash_skipped = 0;
  while (stream->get() == '-') {
    ++num_dash_skipped;
  }
  return num_dash_skipped;
}

void ParseOneLine(int line_index,
                  int width,
                  istream* stream,
                  map<pair<int, int>, int>* numbers) {
  int i = line_index;
  for (int j = 0; j < width; ++j) {
    CHECK_EQ('|', stream->get()) << " at position (" << i << ", " << j << ")";
    CHECK_EQ(' ', stream->get()) << " at position (" << i << ", " << j << ")";

    char cell[3];
    cell[2] = '\0';
    stream->get(cell, 3);
    CHECK_EQ(2, stream->gcount()) << "'" << cell << "'";

    if (cell[0] != ' ') {
      int number = atoi(cell);
      CHECK_GT(number, 0);
      (*numbers)[{i, j}] = number;
    }
  }
  CHECK_EQ('|', stream->get());
  CHECK_EQ('\n', stream->get());
}
}  // namespace

Board ParseBoardFromStream(istream* _stream) {
  istream& stream = *CHECK_NOTNULL(_stream);

  const int width = (SkipLineSeparator(&stream) - 1) / 4;

  map<pair<int, int>, int> numbers;
  for (int line = 0; line < width; ++line) {
    ParseOneLine(line, width, &stream, &numbers);
    SkipLineSeparator(&stream);
  }

  Board board(width, numbers);
  return std::move(board);
}

string Board::ToString(bool debug) const {
  const int line_length = 4 * width_ + 2;  // Include the newline.

  string line_delimiter(line_length, '-');
  line_delimiter[line_length - 1] = '\n';

  ostringstream result;
  for (int i = 0; i < width_; ++i) {
    result << line_delimiter;
    for (int j = 0; j < width_; ++j) {
      result << "| ";
      const Index index = indexer_.GetIndex(i, j);
      const Color color = cells_[index];
      const int number = islands_.GetNumber(index);
      if (number >= 10)                  result << number;
      else if (number > 0)               result << number << " ";
      else if (color == Color::WHITE)    result << (debug ? "· " : "  ");
      else if (color == Color::BLACK)    result << "# ";
      else if (color == Color::UNKNOWN)  result << "  ";

    }
    result << "|\n";
  }
  result << line_delimiter;
  return result.str();
}

