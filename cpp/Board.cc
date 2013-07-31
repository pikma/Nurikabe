#include "Board.h"

#include <stdlib.h>

#include <map>
#include <glog/logging.h>

using namespace std;

Board::Board(int width,
             const map<pair<int, int>, int>& pos_to_numbers)
    : width_(width),
      num_cells_(width_ * width_),
      cells_(num_cells_),
      white_parent_(num_cells_),
      num_white_children_(num_cells_),
      is_valid_(true) {
  CHECK_GT(width_, 0);
  for (const auto& pos_to_number : pos_to_numbers) {
    const pair<int, int>& position = pos_to_number.first;

    const Index index = GetIndex(position.first, position.second);
    for (Index neighbor : GetNeighbors(index)) {
      if (cells_[neighbor].number > 0) {
        is_valid_ = false;
        return;
      }
    }

    CellState& state = cells_[index];
    state.color = WHITE;
    state.number = pos_to_number.second;

    white_parent_[index] = index;
    num_white_children_[index] = 1;
  }
}

void Board::ApplyMove(const Move& move) {
  CHECK(is_valid_);
  cells_[move.index].color = move.color;
  if (move.color == BLACK) {
    if (IsInBlackSquare(move.index) || IsSomeIslandTooSmall(move.index)) {
      is_valid_ = false;
      return;
    }
  } else {
    white_parent_[move.index] = move.index;
    num_white_children_[move.index] = 1;

    for (Index neighbor : GetNeighbors(move.index)) {
      if (cells_[neighbor].color == WHITE)
        MergeWhites(move.index, neighbor);
    }
  }
}

Board::Index Board::GetWhiteRepresentative(Index i) {
  Index parent = white_parent_[i];
  while (parent != i) {
    i = parent;
    parent = white_parent_[i];
  }
  return parent;
}

void Board::MergeWhites(Index i, Index j) {
  if (cells_[i].number > 0 && cells_[j].number > 0) {
    is_valid_ = false;
    return ;
  }

  if (cells_[i].number > 0)
    swap(i, j);

  Index i_repr = GetWhiteRepresentative(i);
  Index j_repr = GetWhiteRepresentative(j);

  // The index i is not tied to an island yet: we merge its group into j's.
  white_parent_[i_repr] = j_repr;
  num_white_children_[j_repr] += num_white_children_[i_repr];

  // If j is tied to a island, we check that it has not become too big.
  if (cells_[j_repr].number > 0 &&
      num_white_children_[j_repr] > cells_[j_repr].number) {
    is_valid_ = false;
  }
}

void Board::GetPosition(Index i, int* x, int* y) const {
  *y = i % width_;
  *x = i / width_;
}

bool Board::IsInBlackSquare(Index black) const {
  int x, y;
  GetPosition(black, &x, &y);

  for (int xx : {x - 1, x + 1}) {
    if (xx < 0 || xx >= width_ || cells_[GetIndex(xx, y)].color != BLACK)
      continue;
    if (y > 0 && cells_[GetIndex(x, y - 1)].color == BLACK &&
        cells_[GetIndex(xx, y - 1)].color == BLACK)
      return true;
    if (y < width_ - 1 && cells_[GetIndex(x, y + 1)].color == BLACK &&
        cells_[GetIndex(xx, y + 1)].color == BLACK)
      return true;
  }
  return false;
}

bool Board::IsSomeIslandTooSmall(Index last_black) const {
  for (Index i = 0; i < num_cells_; ++i) {
    int number = cells_[i].number;
    if (number == 0 || num_white_children_[i] == number) {
      // We skip the cell early if it already has the correct number of white
      // neighbors.
      continue;
    }
    if (GetManhattanDistance(last_black, i) >= number) {
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
    if (cells_[i].color == BLACK) {
      black = i;
      break;
    }
  }
  if (black == -1)
    return false;

  vector<bool> reachable = ReachableCellsNotColored(black, WHITE);

  for (Index i = 0; i < num_cells_; ++i) {
    if (!reachable[i] && cells_[i].color == BLACK)
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
  CHECK_NE(cells_[origin].color, barrier);
  CHECK(!(*reachable)[origin]);

  (*reachable)[origin] = true;
  --max_num_cells;
  int newly_reachable_cells = 1;

  for (Index neighbor : GetNeighbors(origin)) {
    if (max_num_cells == 0)
      break;
    if (cells_[neighbor].color != barrier && !(*reachable)[neighbor]) {
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

int Board::GetManhattanDistance(Index i, Index j) const {
  int xi, yi, xj, yj;
  GetPosition(i, &xi, &yi);
  GetPosition(j, &xj, &yj);
  return abs(xi - xj) + abs(yi - yj);
}

vector<Board::Index> Board::GetNeighbors(Index i) const {
  int x, y;
  GetPosition(i, &x, &y);
  vector<Index> result;
  for (const auto& position : GetNeighbors(x, y)) {
    result.push_back(GetIndex(position.first, position.second));
  }
  return result;
}

vector<pair<int, int>> Board::GetNeighbors(int x, int y) const {
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
