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

// returns 0 if game impossible, otherwise game id
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

int sum_games(ifstream& f) {
	string line;
	int sum = 0;
	while(!f.eof()) {
		getline(f, line);
		sum += min_power(line);
	}

	return sum;
}

int main(){
	ifstream f("2.in");
    int sum = sum_games(f);

	printf("%d\n", sum);
}