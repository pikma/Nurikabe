#include <fstream>
#include <iostream>

#include "Board.h"

using namespace std;

int main(int argc, char** argv) {
  if (argc < 1) {
    cout << "Usage: " << argv[0] << " filename" << endl;
    return 1;
  }

  string filename = argv[1];

  ifstream file;
  file.open(filename);

  Board board = ParseBoardFromStream(&file);
  file.close();

  cout << "Starting board: " << endl;
  cout << board.ToString();
  cout << endl;
  board.Solve();
  if (board.is_valid()) {
    cout << "Solution: " << endl;
  } else {
    cout << "No solutions found. Blocked at: " << endl;
  }
  cout << board.ToString(true);
}
