#include <vector>
#include <cstdio>
#include <numeric>
#include <string>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <cmath>

using namespace std;

class Top {
private:
	int n;
	// Is always sorted. topN[0] > topN[1] > topN[2]... etc.
	vector<int> topN;
public:
	Top(int _n) : n(_n) {
		topN = vector<int>(n, 0);
	}
	void insert(int x) {
		if (x <= topN[n-1]) 
			return;
		// Binary search
		int beg = 0, end = n-1, mid;
		while (beg < end) {
			mid = (beg + end) / 2;
			if (x > topN[mid]) {
				end = mid;
			}
			else {
				beg = mid + 1;
			}
		}
		int tmp;
		for (int i = end; i < n; i++) {
			swap(topN[i], x);
		}
	}

	int sum() {
		return accumulate(topN.begin(), topN.end(), 0, [](int sum, int x){return sum + x;});
	}
};

string digits[] = {"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};

int count_one_elf(ifstream& f){
	int sum = 0;
	int first, last;
	string line = "ffnrprtnine1tjznmckv5sixczv";
	if(f.is_open()) {
		while(f) {
			getline(f, line);
			bool found;
			for (int i = 0; i < line.size(); i++) {
				if (line[i] >= '0' && line[i] <= '9') {
					first = (int) line[i] - int('0');
					//cout << "front found " << first << " at " << i << "\n";
					break;
				}
				else {
					found = false;
					for (int j = 1; j<=9; j++) {
						string digit = digits[j-1];
						int n = digit.length();
						if (line.substr(i, n) == digit) {
							first = j;
							//printf(" firstD: %d ", first);
							found = true;
							//cout << "WORD front found " << first << "at " << i << "\n";
							break;
						}
					}
				}	
				if (found) 
					break;
			}
			for (int i = line.size()-1; i >= 0; i--) {
				if (line[i] >= '0' && line[i] <= '9') {
					
					last = (int) line[i] - int('0');
					//cout << "back found " << last << "at " << i << "\n";
					break;
				}
				else {
					found = false;
					for (int j = 1; j<=9; j++) {
						string digit = digits[j-1];
						int n = digit.length();
						if (line.substr(i, n) == digit) {
							last = j;
							//printf(" lastD: %d ", last);
							found = true;
							//cout << "WORD back found " << last << "at " << i << "\n";
							break;
						}
					}
				}
				if (found) 
					break;
			}
			//cout << "line:\n" << "first: " << first << "\n" << last << "\n";
			printf(" :%d\n", 10*first + last);
			cout << line << "\n";
			sum += 10*first + last;
		}
	}
	return sum;
}

int main(){
	ifstream f("1.in");

    int sum = 0;
    while(f) {
    	sum += count_one_elf(f);
    }


	printf("%d\n", sum);
}