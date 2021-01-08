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


vector<int> find_two_values_with_sum_in_vector(vector<int> entries, int sum) {
  vector<int> values;
  int lowest_index = 0;
  int highest_index = 0;
  int lowest_value = 0;
  int highest_value = 0;

  highest_index = (int)entries.size() - 1;

  bool found = false;

  do {
    lowest_value = entries.at(lowest_index);
    highest_value = entries.at(highest_index);
//    cout << "lowets L: " << lowest_value << endl;
//    cout << "hiiest L: " << highest_value << endl;
//    cout << "lowest Index: " << lowest_index << endl;
//    cout << "highest Index: " << highest_index << endl;

    if(lowest_value + highest_value == sum){
      found = true;
      break;
    }

    if(lowest_value + highest_value < sum){
      lowest_index++;
      continue;
    }
    if(lowest_value + highest_value > sum){
      highest_index--;
      continue;
    }
  }
  while(lowest_index < entries.size()-1 && highest_index > 0);

//  cout << "lowets L: " << lowest_value << endl;
//  cout << "hiiest L: " << highest_value << endl;

  if(found){
    cout << "MATCH!!" << endl;
    values.push_back(lowest_value);
    values.push_back(highest_value);
  }

  return values;
}
