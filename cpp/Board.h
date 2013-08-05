#include <map>
#include <stack>
#include <utility>
#include <vector>

typedef int Index;

enum Color {
  BLACK,
  WHITE,
  UNKNOWN,
};

struct Move {
  int index;
  Color color;
};

class Indexer {
 public:
  Indexer(int width) : width_(width) {}

  Index GetIndex(int x, int y) const { return x * width_ + y; }
  void GetPosition(Index i, int* x, int* y) const;

  std::vector<Index> GetNeighbors(Index i) const;
  std::vector<std::pair<int, int>> GetNeighbors(int x, int y) const;

  int GetManhattanDistance(Index i, Index j) const;

 private:
  const int width_;
};

class IslandSet {
 public:
  IslandSet(
      int width, const std::map<std::pair<int, int>, int>& pos_to_numbers);

  // Returns false if setting the cell to white either:
  //   - creates one island with two numbers;
  //   - creates an island with too many white cells.
  bool SetWhite(Index i);
  void Undo(Index i);

  // Returns 0 if the cell is not a number.
  int GetNumber(Index i) const { return numbers_[i]; }

  bool GetIslandCurrentSize(Index i) const { return num_white_children_[i]; }

 private:
  // The two cells must be white. Returns false if it either:
  //   - creates one island with two numbers;
  //   - creates an island with too many white cells.
  bool MergeWhites(Index newly_white, Index white_neigbor);

  // Returns -1 is the cell i is not white.
  Index GetWhiteRepresentative(Index i) const;

  struct UndoEdit {
    Index newly_white;
    Index white_parent_index;
    int white_parent_previous_value;
    Index num_white_children_index;
    int num_white_children_previous_value;
  };

  const Indexer indexer_;
  std::vector<int> white_parent_;
  std::vector<int> num_white_children_;
  std::stack<UndoEdit> last_edits_;
  std::vector<int> numbers_;
};

class Board {
 public:
  Board(int width,
        const std::map<std::pair<int, int>, int>& pos_to_numbers);

  void ApplyMove(const Move& move);
  void UndoMove(const Move& move);

  void SolveWithoutGuess();

 private:
  // When making a move at position p, it makes the states invalid iff it
  // breaks any of the conditions below, depending on its color.
  //
  // White:
  // 1. Connects two islands with whites.
  // 2. Make a white island too big.
  //
  // Black:
  // 1. Makes an island too small.
  // 2. Creates a black square.
  //
  // Both:
  // 1. Isolates a portion of the black river.

  bool IsInBlackSquare(Index black) const;

  // Returns true if the last cell that became black restricted the size of
  // some island so that their size is too small.
  bool IsSomeIslandTooSmall(Index last_black) const;

  bool AreBlackDisconnected() const;

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

  bool IsMoveValid(const Move& move) const;

  const int width_;
  const int num_cells_;
  const Indexer indexer_;
  std::vector<Color> cells_;
  IslandSet islands_;
  bool is_valid_;
};
