/*
 * Find 3 entries that sum to 2020
 * Sort the entries
 * With pointers at beginning and end,
 * if they sum to larger or equal to 2020
 * lower the end.
 * If less, subtract the sum from 2020
 *    Find 2 entries that sum to the difference using the part1 technique
 * If 2 entries not found, raise the beginning
 */

#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <algorithm>
#include "help.h"

using namespace std;

string line;
vector<int> entries;
int lowest_index = 0;
int highest_index = 0;
int lowest_value = 0;
int highest_value = 0;

int main(int argc, char ** argv){
  cout << "Processing input file: " << argv[1] << endl;
  fstream infile(argv[1]);
  if(false == infile.is_open()){
    cout << "FAILED TO OPEN";
    exit(-1);
  }

  read_lines_ints(infile, entries);


  sort(entries.begin(),entries.end());

  for(auto v: entries){
    cout << v << endl;
  }
  highest_index = entries.size() - 1;

  do {
    lowest_value = entries.at(lowest_index);
    highest_value = entries.at(highest_index);

    int sum = lowest_value + highest_value;

    if(sum >= 2020){
      highest_index--;
      continue;
    }

    int difference = 2020 - sum;

    cout << "Lowest: " << lowest_value << endl;
    cout << "Highest: " << highest_value << endl;
    cout << "Diff: " << difference << endl;

    bool found = binary_search(entries.begin(), entries.end(), difference);
    if(found){
      break;
    }

    lowest_index++;


  }
  while(lowest_index < entries.size()-1);

  cout << "done" << endl;
}
