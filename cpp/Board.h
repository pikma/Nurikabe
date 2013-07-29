#include <map>
#include <vector>

enum Color {
  BLACK,
  WHITE,
  UNKNOWN,
};

struct CellState {
  // The number is zero for cells that don't have a number.
  short number;
  //  The color is white for cells that have a number.
  Color color;
};

struct Move {
  int index;
  Color color;
};

class Board {
 public:
   Board(int width,
         const std::map<std::pair<int, int>, int>& pos_to_numbers);


 private:
   typedef int Index;

   Index GetIndex(int x, int y) const { return x * width_ + y; }
   Index MaxIndex() const { return width_ * width_; }

   // When making a move at position p, it makes the states invalid iff it
   // breaks any of the conditions below, depending on its color.
   //
   // White:
   // 1. Connects two islands with whites.
   // 2. Make a white island too big.
   // 3. Isolates a portion of the black river.
   //
   // Black:
   // 1. Makes an island too small.
   // 2. Creates a black square.

   bool HasBlackSquare() const;

   const int width_;
   std::vector<CellState> cells_;
};
