/*
 * Find 3 entries that sum to 2020
 * Sort the entries
 * With pointer at the end,
 * subtract the value from 2020
 *    Find 2 entries that sum to the difference using the part1 technique
 *    if match, done
 * lower the end
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

//  for(auto v: entries){
//    cout << v << endl;
//  }
//
  highest_index = entries.size() - 1;

  do {
    //lowest_value = entries.at(lowest_index);
    highest_value = entries.at(highest_index);

//    int sum = lowest_value + highest_value;
//
//    if(sum >= 2020){
//      highest_index--;
//      continue;
//    }

    int difference = 2020 - highest_value;

//    cout << "Lowest: " << lowest_value << endl;
    cout << "Highest: " << highest_value << endl;
    cout << "Diff: " << difference << endl;

    vector<int> subvector(entries.begin(), entries.begin()+highest_index);

    cout << "Searching subvector" << endl;
    cout << "length of subvector : " << subvector.size();
    cout << "length of orig : " << entries.size() << endl;
    vector<int> result = find_two_values_with_sum_in_vector(subvector, difference);
    if(result.size()){
      cout << "YAY FOUND" << endl;
      cout << result.at(0) << endl;
      cout << result.at(1) << endl;
      break;
    }

    highest_index--;
  }
  while(highest_index > 0);

  cout << "done" << endl;
}
