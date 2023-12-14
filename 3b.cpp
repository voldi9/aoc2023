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

bool find_number_at(const string& row, int pos, vector<int>& numbers) {
	if(pos <= 0 || pos >= row.size() - 1 || !is_digit(row[pos])) {
		return false;
	}
	while (pos > 0 && is_digit(row[pos - 1])) { pos--; }

	int x = 0;
	while (pos < row.size() && is_digit(row[pos])) {
		x = x*10 + int(row[pos++]) - int('0');
	}
	numbers.push_back(x);
	return true;
}

void find_in_row(const string& row, int pos, vector<int>& numbers) {
	if(find_number_at(row, pos, numbers))
		return;
	find_number_at(row, pos - 1, numbers);
	find_number_at(row, pos + 1, numbers);
}

int check_rows(const string& prev, const string& cur, const string& next) {
	int sum = 0;
	vector<int> numbers;
	for(int i = 0; i < cur.size(); i++) {
		if (cur[i] == '*') {
			numbers = vector<int>{};
			find_in_row(cur, i, numbers);
			find_in_row(prev, i, numbers);
			find_in_row(next, i, numbers);
			if(numbers.size() == 2) {
				sum += numbers[0] * numbers[1];
			}
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
	} 
	string last_row = string(cur.size(), '.');
	sum += swap_vecs(prev, cur, next, last_row);

	return sum;
}

int main(){
	ifstream f("3.in");
    int sum = sum_parts(f);

	printf("%d\n", sum);
}