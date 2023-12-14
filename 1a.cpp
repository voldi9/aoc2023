#include <vector>
#include <cstdio>
#include <string>
#include <algorithm>
#include <iostream>
#include <fstream>

using namespace std;

int main(){
	ifstream f("1.in");
	string line;

    int max = 0, sum = 0;

	if(f.is_open()) {
		while(f) {
			getline(f, line);
			if(line == "") {
				printf("%d\n", sum);
				if(sum > max) {
					max = sum;
				}
				sum = 0;
			}
			else {
				sum += stoi(line);
			}
		}
	}
	printf("%d", max);
}