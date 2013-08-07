#include "Board.h"

#include <stdlib.h>

#include <map>
#include <glog/logging.h>

using namespace std;

Color OppositeColor(Color c) {
  switch (c) {
    case BLACK:   return WHITE;
    case WHITE:   return BLACK;
    case UNKNOWN: LOG(FATAL) << "OppositeColor(UNKNOWN) is undefined.";
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

int Indexer::GetManhattanDistance(Index i, Index j) const {
  int xi, yi, xj, yj;
  GetPosition(i, &xi, &yi);
  GetPosition(j, &xj, &yj);
  return abs(xi - xj) + abs(yi - yj);
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
    SetWhite(i);
  }
}

bool IslandSet::SetWhite(Index i) {
  CHECK_EQ(-1, white_parent_[i]);
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
  // Some changes do not generate any edits as MergeWhites detects that they
  // lead to an invalid state and returns early. Hence it is possible that the
  // last_edits_ stack is empty.
  while (!last_edits_.empty() && last_edits_.top().newly_white == i) {
    const UndoEdit& undo = last_edits_.top();
    white_parent_[undo.white_parent_index] = undo.white_parent_previous_value;
    num_white_children_[undo.num_white_children_index] =
      undo.num_white_children_previous_value;
    last_edits_.pop();
  }
}

// *****************************************************************************
// Board.

Board::Board(int width,
             const map<pair<int, int>, int>& pos_to_numbers)
    : width_(width),
      num_cells_(width_ * width_),
      indexer_(width_),
      cells_(num_cells_, UNKNOWN),
      islands_(width, pos_to_numbers),
      is_valid_(true) {
  CHECK_GT(width_, 0);
  for (const auto& pos_to_number : pos_to_numbers) {
    const pair<int, int>& position = pos_to_number.first;
    cells_[indexer_.GetIndex(position.first, position.second)] = WHITE;
  }
}

void Board::ApplyMove(const Move& move) {
  CHECK(is_valid_);
  CHECK_EQ(UNKNOWN, cells_[move.index]);

  cells_[move.index] = move.color;
  switch (move.color) {
    case BLACK:
      if (IsInBlackSquare(move.index) || IsSomeIslandTooSmall(move.index)) {
        is_valid_ = false;
        return;
      }
    case WHITE:
      if (!islands_.SetWhite(move.index)) {
        is_valid_ = false;
        return;
      }
    default:
      LOG(FATAL) << "Invalid move " << move.index << " --> " << move.color;
  }
  if (AreBlackDisconnected()) {
    is_valid_ = false;
    return;
  }
}

bool Board::IsInBlackSquare(Index black) const {
  int x, y;
  indexer_.GetPosition(black, &x, &y);

  for (int xx : {x - 1, x + 1}) {
    if (xx < 0 || xx >= width_ || cells_[indexer_.GetIndex(xx, y)]!= BLACK)
      continue;
    if (y > 0 && cells_[indexer_.GetIndex(x, y - 1)] == BLACK &&
        cells_[indexer_.GetIndex(xx, y - 1)] == BLACK)
      return true;
    if (y < width_ - 1 && cells_[indexer_.GetIndex(x, y + 1)] == BLACK &&
        cells_[indexer_.GetIndex(xx, y + 1)] == BLACK)
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
    if (indexer_.GetManhattanDistance(last_black, i) >= number) {
      // The cell that was marked black is too far to affect this island.
      continue;
    }

    vector<bool> reachables = ReachableCellsNotColoredAtMost(i, BLACK, number);
    if (reachables.size() < number)
      return true;
  }
  return false;
}

bool Board::AreBlackDisconnected() const {
  Index black = -1;
  for (Index i = 0; i < num_cells_; ++i) {
    if (cells_[i] == BLACK) {
      black = i;
      break;
    }
  }
  if (black == -1)
    return false;

  vector<bool> reachable = ReachableCellsNotColored(black, WHITE);

  for (Index i = 0; i < num_cells_; ++i) {
    if (!reachable[i] && cells_[i] == BLACK)
      return true;
  }
  return false;
}

vector<bool> Board::ReachableCellsNotColored(
    Index origin, Color barrier) const {
  return ReachableCellsNotColoredAtMost(origin, barrier, num_cells_);
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

vector<bool> Board::ReachableCellsNotColoredAtMost(
    Index origin, Color barrier, int max_num_cells) const {
  vector<bool> reachable(num_cells_);
  MarkAndPropagate(origin, barrier, max_num_cells, &reachable);
  return reachable;
}

void Board::UndoMove(const Move& move) {
  CHECK_NE(UNKNOWN, move.color);
  CHECK_EQ(move.color, cells_[move.index]);

  cells_[move.index] = UNKNOWN;
  if (move.color == WHITE)
    islands_.Undo(move.index);

  // It is not possible to apply a move when the state is invalid, thus we know
  // that the previous state was valid, whether the current state is valid or
  // not.
  is_valid_ = true;
}

bool Board::IsMoveValid(const Move& move) const {
  Board* that = const_cast<Board*>(this);
  that->ApplyMove(move);
  bool result = is_valid_;
  that->UndoMove(move);
  return result;
}

bool Board::SolveWithoutGuess(stack<Move>* applied_moves) {
  bool made_progress;
  do {
    made_progress = false;
    for (Index i = 0; i < num_cells_; ++i) {
      if (cells_[i] != UNKNOWN)
        continue;
      if (!is_valid_)
        return false;

      const Move black_move = {i, BLACK};
      const Move white_move = {i, WHITE};
      if (IsMoveValid(black_move)) {
        if (!IsMoveValid(white_move)) {
          ApplyMove(black_move);
          applied_moves->push(black_move);
          made_progress = true;
        }
      } else {
        ApplyMove(white_move);
        applied_moves->push(white_move);
        made_progress = true;
      }
    }
  } while (made_progress);
  return is_valid_;
}

bool Board::Solve() {
  if (!is_valid_)
    return false;

  stack<Move> applied_moves;
  SolveWithoutGuess(&applied_moves);

  Move next_move = GetNextGuessMove();
  if (SolveWithGuess(next_move))
    return true;

  next_move.color = OppositeColor(next_move.color);
  if (SolveWithGuess(next_move))
    return true;

  while (!applied_moves.empty()) {
    UndoMove(applied_moves.top());
    applied_moves.pop();
  }
  return false;
}

bool Board::SolveWithGuess(const Move& next_move) {
  if (!is_valid_)
    return false;

  ApplyMove(next_move);
  if (Solve()) {
    return true;
  } else {
    UndoMove(next_move);
    return false;
  }
}

Move Board::GetNextGuessMove() const {
  int max_num_known_neighbors = -1;
  int bestIndex = -1;
  for (Index i = 0; i < num_cells_; ++i) {
    if (cells_[i] != UNKNOWN)
      continue;

    int num_known_neighbors = 0;
    for (Index neighbor : indexer_.GetNeighbors(i)) {
      if (cells_[neighbor] != UNKNOWN)
        ++num_known_neighbors;
    }

    if (num_known_neighbors == 4) {
      bestIndex = i;
      break;
    }
    if (num_known_neighbors > max_num_known_neighbors) {
      max_num_known_neighbors = num_known_neighbors;
      bestIndex = i;
    }
  }
  return {bestIndex, BLACK};
}

