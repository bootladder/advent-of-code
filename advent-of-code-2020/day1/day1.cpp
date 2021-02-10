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

  auto twovalues = find_two_values_with_sum_in_vector(entries, 2020);

  cout << twovalues.at(0) << endl;
  cout << twovalues.at(1) << endl;
}


