//
// Created by steve on 1/8/21.
//

#include <iostream>
#include <string>
#include <fstream>
#include <vector>

using namespace std;

bool is_valid_password(string line);

void load_index1_index2(int & max, int & min, basic_string<char> &basicString);

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

  int index1, index2;
  load_index1_index2(index1,index2,results.at(0));
  cout << "load_index1 is : " << index1 << endl;
  cout << "load_index1 is : " << index2 << endl;

  char c = results.at(1).at(0);
  cout << "letter CHAR is : " << c << endl;

  string password = results.at(2);
  cout << "password is : " << password << endl;
  return is_valid_password(index1,index2,c,password);
}

void load_index1_index2(int & index1, int & index2, basic_string<char> &rangeStr) {
  vector<string> results;
  boost::split(results, rangeStr, [](char c){return c == '-';});

  index1 = stoi(results.at(0));
  index2 = stoi(results.at(1));
}

bool is_valid_password(int index1, int index2, char c, string password) {
  int count = 0;
  if(password.at(index1-1) == c){
    count++;
  }
  if(password.at(index2-1) == c){
    count++;
  }
  return count == 1;
}
