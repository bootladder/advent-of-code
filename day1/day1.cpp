/*
 * Find 2 entries that sum to 2020
 * Sort the entries
 * Starting with 2 pointers, at beginning and end,
 * narrow into the middle until they sum to 2020
 * if the 2 sum to larger than 2020, lower the higher one,
 * if less, raise the lower one,
 * if equal, done.
 */

#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <algorithm>
#include "help.h"
using namespace std;

int main(int argc, char ** argv){
  vector<int> entries;
  int lowest_index = 0;
  int highest_index = 0;
  int lowest_value = 0;
  int highest_value = 0;

  cout << "Processing input file: " << argv[1] << endl;
  fstream infile(argv[1]);
  if(false == infile.is_open()){
    cout << "FAILED TO OPEN";
    exit(-1);
  }

  read_lines_ints(infile, entries);

  sort(entries.begin(),entries.end());

//  for(auto v : entries){
//    cout << v << endl;
//  }

  highest_index = (int)entries.size() - 1;

  do {
    lowest_value = entries.at(lowest_index);
    highest_value = entries.at(highest_index);
//    cout << "lowets L: " << lowest_value << endl;
//    cout << "hiiest L: " << highest_value << endl;

    if(lowest_value + highest_value == 2020){
      break;
    }

    if(lowest_value + highest_value < 2020){
      lowest_index++;
      continue;
    }
    if(lowest_value + highest_value > 2020){
      highest_index--;
      continue;
    }

  }
  while(lowest_index < entries.size()-1);

  cout << "lowets L: " << lowest_value << endl;
  cout << "hiiest L: " << highest_value << endl;
}


