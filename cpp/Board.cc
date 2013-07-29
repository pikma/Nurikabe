#include "Board.h"

#include <glog/logging.h>

using namespace std;

Board::Board(int width,
             const map<pair<int, int>, int>& pos_to_numbers)
    : width_(width),
      cells_(width * width) {
  CHECK_GT(width_, 0);
  for (const auto& pos_to_number : pos_to_numbers) {
    const pair<int, int>& position = pos_to_number.first;
    CellState& state = cells_[GetIndex(position.first, position.second)];
    state.color = WHITE;
    state.number = pos_to_number.second;
  }
}

bool Board::HasBlackSquare() const {
  for (int i = 0; i < width_ - 1; ++i) {
    for (int j = 0; j < width_ - 1; ++j) {
      if (cells_[GetIndex(i,     j    )].color == BLACK &&
          cells_[GetIndex(i + 1, j    )].color == BLACK &&
          cells_[GetIndex(i,     j + 1)].color == BLACK &&
          cells_[GetIndex(i + 1, j + 1)].color == BLACK) {
        return true;
      }
    }
  }
  return false;
}
