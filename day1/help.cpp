//
// Created by steve on 1/8/21.
//

#include "help.h"
void read_lines_ints(fstream& fstream, vector<int> & entries) {
  string line;

  while(getline(fstream, line)){
    if(line.empty()) {
      break;
    }
    int i = stoi(line);
    entries.insert(entries.end(),i);
  }
}