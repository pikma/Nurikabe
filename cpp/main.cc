#include <fstream>
#include <iostream>

#include <glog/logging.h>

#include "Board.h"

using namespace std;

int main(int argc, char** argv) {
  google::InitGoogleLogging(argv[0]);
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
  if (board.Solve()) {
    cout << "Solution: " << endl;
    cout << board.ToString();
  } else {
    cout << "No solutions found." << endl;
  }
}
