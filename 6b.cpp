#include <vector>
#include <cstdio>
#include <numeric>
#include <string>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>
#include <regex>
#include <map>

using namespace std;

const double epsilon = 0.0000001;

struct Race {
      long long time, distance;
      Race(long long t, long long d) : time(t), distance(d) {}
};

long long read_line(ifstream& f, int cut) {
	string s;
	getline(f, s);
	long long result = 0;
	stringstream ss(s.substr(cut, s.length() - cut));
	while(!ss.eof()) {
	   ss >> s;
	   result = pow(10, s.length()) * result + stoi(s);
	}
	return result;
}

Race read_input(ifstream& f) {
	return Race(read_line(f, 6), read_line(f, 10));
}

long long calculate(const Race r) {
	long long delta =  (long long) r.time * r.time -  (long long) 4*r.distance;
	if (delta < 0) 
      	return 0;
      if (delta == 0)
      	return 1;
  	double sqd = sqrt(delta);
  	//printf(" ! %lld\n", delta);
  	//printf("%lf - %lf = %lld\n", floor((r.time+  sqd)/2), floor((r.time - sqd)/2) ,(long long) floor((r.time + sqd)/2) - (long long) ceil((r.time - sqd)/2) + 1);
	return (long long) floor((r.time + sqd)/2 - epsilon) - (long long) ceil((r.time - sqd)/2 + epsilon) + 1;
}

int main(){
	ifstream f("6.in");
	printf("%lld\n", calculate(read_input(f)));
	return 0;
}