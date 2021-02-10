//
// Created by steve on 1/8/21.
//

#include "filehelp.h"
using namespace std;
void read_fstream_lines(fstream & infile, vector<string> & lines){
  string line;

  while(getline(infile, line)){
    if(line.empty())
      break;

    lines.push_back(line);
  }

}