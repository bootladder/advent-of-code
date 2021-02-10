#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include "filehelp.h"

using namespace std;

int count_trees_by_slope(vector<string> & lines, int down, int right);

void print_trees_slope(vector<string> & lines, int down, int right);

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

  int down = 1, right = 3;
  int numtrees = count_trees_by_slope(lines, down, right);
  cout << "Num trees: " << numtrees << endl;

  print_trees_slope(lines,1, 1);
  print_trees_slope(lines,1, 3);
  print_trees_slope(lines,1, 5);
  print_trees_slope(lines,1, 7);
  print_trees_slope(lines,2, 1);

}


void print_trees_slope(vector<string> & lines, int down, int right) {
  cout << "Down " << down << "Right " << right << " : ";
  int numtrees = count_trees_by_slope(lines, down,right);
  cout << numtrees << endl;
}

int count_trees_by_slope(vector<string> & lines, int down, int right) {
  int numtrees = 0;
  int linelength = lines.at(0).size();
  int charindex = 0  - right;
  int downcounter = 1;

  int linecounter = 0 - down; //debug

  // starting at the top, there is no tree
  // so the first tree processed in the loop is down and right
  for(auto line: lines){
    //go down
    if(downcounter < down-1){
      downcounter++;
      continue;
    }
    downcounter = 0;
    linecounter+= down;

    //go right
    charindex+=right;
    charindex = charindex % linelength;

    //check
    if(line.at(charindex) == '#'){
      numtrees+=1;
      cout << "Tree: " << "line " << linecounter << " , " << "char " << charindex << endl;
    }

  }

  return numtrees;
}
