//
// Created by steve on 1/8/21.
//

#include "day2-1.h"

#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <algorithm>

using namespace std;

bool is_valid_password(string line);

void load_max_min(int & max, int & min, basic_string<char> &basicString);

bool is_valid_password(int i, int i1, char i2, string basicString);

int main(int argc, char ** argv) {
  cout << "Processing input file: " << argv[1] << endl;
  fstream infile(argv[1]);
  if (false == infile.is_open()) {
    cout << "FAILED TO OPEN";
    exit(-1);
  }

  int numvalidpasswords = 0;
  string line;

  while(getline(infile, line)){
    if(line.empty())
      break;

    if(is_valid_password(line))
      numvalidpasswords++;
  }

  cout << "Num valid passwords : " << numvalidpasswords;
}

/*
1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
 */

#include <boost/algorithm/string.hpp>


bool is_valid_password(string line) {
  std::vector<std::string> results;
  boost::split(results, line, [](char c){return c == ' ';});

//  cout << "count range : " << results.at(0) << endl;
  int max, min;
  load_max_min(max,min,results.at(0));
  cout << "max is : " << max << endl;
  cout << "min is : " << min << endl;

//  cout << "letter : " << results.at(1) << endl;

  char c = results.at(1).at(0);
  cout << "letter CHAR is : " << c << endl;

  string password = results.at(2);
  cout << "password is : " << password << endl;
  return is_valid_password(min,max,c,password);
}

void load_max_min(int & max, int & min, basic_string<char> &rangeStr) {
  std::vector<std::string> results;
  boost::split(results, rangeStr, [](char c){return c == '-';});

  min = stoi(results.at(0));
  max = stoi(results.at(1));
}

bool is_valid_password(int min, int max, char c, string password) {
  // count number of c's in password
  int count = 0;
  for(auto ctrav : password){
//    cout << ctrav << endl;
    if(ctrav == c)
      count++;
  }
  cout << "count is : " << count << endl;
  return (count >= min && count <= max);
}
