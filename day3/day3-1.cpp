#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include "filehelp.h"

using namespace std;

int main(int argc, char ** argv) {
  cout << "Processing input file: " << argv[1] << endl;
  fstream infile(argv[1]);
  if (false == infile.is_open()) {
    cout << "FAILED TO OPEN";
    exit(-1);
  }

  vector<string> lines;
  read_fstream_lines(infile, lines);

  cout << "Num lines : " << lines.size() << endl;
  cout << "Size of each line: " << lines.at(0).size() << endl;

  int numtrees = 0;
  int linelength = lines.at(0).size();
  int charindex = 0;
  for(auto line: lines){
    if(line.at(charindex) == '#')
      numtrees++;

    charindex+=3;
    if(charindex >= linelength)
      charindex = charindex % linelength;
  }

  cout << "Num trees: " << numtrees << endl;
}