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

map<string, int> max_numbers = { { "red", 12},
 								 { "green", 13 },
 								 { "blue", 14 } };

bool is_round_possible(const string& round) {
	smatch match;
	const auto end = std::sregex_token_iterator();
    auto it = std::sregex_token_iterator(round.begin(), round.end(), scolor, -1);
    while(it != end) {
		string single = (string) *it++;
		regex_search(single, match, ssingle);
		if (stoi(match[1]) > max_numbers[match[2]])
		 	return false;
	}
	return true;
}

// returns 0 if game impossible, otherwise game id
int is_game_possible(string& line) {
	smatch match;
	regex_match(line, match, game);
	int id = stoi(match[1]);
	line = match[2];
	const auto end = std::sregex_token_iterator();
    auto it = std::sregex_token_iterator(line.begin(), line.end(), sround, -1);
 	bool possible = true;

	while(it != end) {
		if(!is_round_possible(*it++)) {
			possible = false;
			break;
		}
	}
	
	if (possible)
		return id;
	return 0;
}

int sum_games(ifstream& f) {
	string line;
	int sum = 0;
	while(!f.eof()) {
		getline(f, line);
		sum += is_game_possible(line);
	}

	return sum;
}

int main(){
	ifstream f("2.in");
    int sum = sum_games(f);

	printf("%d\n", sum);
}