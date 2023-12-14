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
      int time, distance;
      Race() {}
};

void read_line(ifstream& f, vector<Race>& races, int cut, bool is_time) {
	string s;
	int i = 0;
	getline(f, s);
	stringstream ss(s.substr(cut, s.length() - cut));
	while(!ss.eof()) {
	   ss >> s;
	   if(is_time) {
		   Race r{};
		   r.time = stoi(s);
	   	   races.push_back(r);
	   	}
	   	else {
	   		races[i++].distance = stoi(s);
	   	}
	}
}

void read_input(ifstream& f, vector<Race>& races) {
	read_line(f, races, 6, true);
	read_line(f, races, 10, false);
}

long long calculate(vector<Race>& races) {
	long long result = 1;
	for (Race& r : races) {
	    long long delta = r.time * r.time - 4*r.distance;
	    if (delta < 0) 
	        return 0;\
	    if(delta == 1)
	    	continue;
	  	double sqd = sqrt(delta);
	  	//printf(" ! %lld\n", delta);
	  	//printf("%lf - %lf = %lld\n", floor((r.time+  sqd)/2), floor((r.time - sqd)/2) ,(long long) floor((r.time + sqd)/2) - (long long) ceil((r.time - sqd)/2) + 1);
	    result *= (long long) floor((r.time + sqd)/2 - epsilon) - (long long) ceil((r.time - sqd)/2 + epsilon) + 1;
	}
	return result;
}

int main(){
	ifstream f("6.in");
	vector<Race> races{};

	read_input(f, races);
	printf("%lld\n", calculate(races));
}