#include <iostream>
#include <map>
#include <string>
#include <utility>
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
   Board(Board&&) = default;

   // Fills the board until either the board is full (a solution has been found)
   // or the state is invalid (there is no solution).
   void Solve();

   std::string ToString(bool debug = false) const;

   bool IsSolved() const;

 private:
   typedef int Index;

   Index GetIndex(int x, int y) const { return x * width_ + y; }
   void GetPosition(Index i, int* x, int* y) const;


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

   bool IsInBlackSquare(Index black) const;

   // Returns true if the last cell that became black restricted the size of
   // some island so that their size is too small.
   bool IsSomeIslandTooSmall(Index last_black) const;

   bool AreBlackDisconnected() const;

   std::vector<Index> GetNeighbors(Index i) const;
   std::vector<std::pair<int, int>> GetNeighbors(int x, int y) const;

   void ApplyMove(const Move& move);

   // Connects two white cells that were not previously connected. Returns
   // false if that results in an invalid state, i.e. if a white island is now
   // too big, or now contains two numbers.
   bool MergeWhites(Index i, Index j);

   // The cell j must be white.
   Index GetWhiteRepresentative(Index i);

   // Returns the cells reachable from the origin by traversing only cells that
   // are not of the barrier color.
   std::vector<bool> ReachableCellsNotColored(
       Index origin, Color barrier) const;

   // Same as above, but stops as soon as it find max_num_cells reachable cells.
   std::vector<bool> ReachableCellsNotColoredAtMost(
       Index origin, Color barrier, int max_num_cells) const;

   // Marks the origin as reachable, and propagates to its neighbors. Returns
   // the number of cells newly marked as reachable.
   int MarkAndPropagate(Index origin,
                        Color barrier,
                        int max_num_cells,
                        std::vector<bool>* reachable) const;

   int GetManhattanDistance(Index i, Index j) const;

   void AddWhiteToWhiteAreas(Index i);

   const int width_;
   const int num_cells_;
   std::vector<CellState> cells_;
   std::vector<int> white_parent_;
   std::vector<int> num_white_children_;
   bool is_valid_;
   int num_unknown_cells_;

   Board(const Board&);
   const Board& operator=(const Board&);
};

Board ParseBoardFromStream(std::istream* stream);
