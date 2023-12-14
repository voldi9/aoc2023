#include <vector>
#include <cstdio>
#include <numeric>
#include <string>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <cmath>
#include <regex>
#include <map>

using namespace std;

const regex color { "(\\d) (blue|red|green),?" };
const regex sround { "; " };
const regex scolor { "," };
const regex ssingle { "(\\d+) (red|blue|green)" };
const regex game { "^Game (\\d+): (.*$)" };

map<string, int> max_numbers;



bool update_powers(const string& round) {
	smatch match;
	const auto end = std::sregex_token_iterator();
    auto it = std::sregex_token_iterator(round.begin(), round.end(), scolor, -1);
    while(it != end) {
		string single = (string) *it++;
		regex_search(single, match, ssingle);
		max_numbers[match[2]] = max(max_numbers[match[2]], stoi(match[1]));
	}
	return true;
}

int min_power(string& line) {
	max_numbers["red"] = max_numbers["green"] = max_numbers["blue"] = 0;
	smatch match;
	regex_match(line, match, game);
	int id = stoi(match[1]);
	line = match[2];
	const auto end = std::sregex_token_iterator();
    auto it = std::sregex_token_iterator(line.begin(), line.end(), sround, -1);

	while(it != end) {
		update_powers(*it++);
	}
	
	return max_numbers["red"] * max_numbers["green"] * max_numbers["blue"];
}

inline bool is_digit(char c) {
	return c >= '0' && c <= '9';
}

inline bool is_symbol(char c) {
	return c != '.' && !is_digit(c);
}

int check_rows(const string& prev, const string& cur, const string& next) {
	bool counts;
	int x, fpos, lpos, sum = 0;
	cout << "looking at: " << cur << "\n";
	for(int i = 0; i < cur.size(); i++) {
		x = 0;
		counts = false;
		while (is_digit(cur[i])) {
			x = x*10 + int(cur[i]) - int('0');
			counts = counts || (i > 0 && is_symbol(cur[i-1])) || (i < cur.size() - 1 && is_symbol(cur[i+1]));
			counts = counts || is_symbol(next[i]) || (i > 0 && is_symbol(next[i-1])) || (i < next.size() - 1 && is_symbol(next[i+1]));
			counts = counts || is_symbol(prev[i]) || (i > 0 && is_symbol(prev[i-1])) || (i < prev.size() - 1 && is_symbol(prev[i+1]));
			i++;
		}
		if(counts) {
			sum += x;
			cout << "adding " << x << "\n";
		}
	}
	return sum;
}

int swap_vecs(string& prev, string& cur, string& next, string& input) {
	prev = std::move(cur);
	cur = std::move(next);
	next = std::move(input);
	return check_rows(prev, cur, next);
}

int sum_parts(ifstream& f) {
	long long sum = 0;
	char c;
	string prev, cur, next, input;
	getline(f, next);
	cur.resize(next.size(), '.');

	while (!f.eof()) {
		getline(f, input);
		sum += swap_vecs(prev, cur, next, input);
		// sum += check_rows(prev, cur, next);
	} 
	string last_row = string(cur.size(), '.');
	sum += swap_vecs(prev, cur, next, last_row);
	// sum += check_rows(prev, cur, next);
	return sum;
}

int main(){
	ifstream f("3.in");
    int sum = sum_parts(f);

	printf("%d\n", sum);
}