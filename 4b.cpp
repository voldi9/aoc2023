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

const regex rcard { "^Card\\s+\\d+: (.*) \\| (.*)$" };
const regex split { "\\s+" };

int multipliers[300];

int iterate_nums(map<int, bool>& is_winning, std::sregex_token_iterator it, sregex_token_iterator end, bool are_winning) {
	int wins = 0;
    while ( it != end ) {
    	string num = *it++;
    	if (num.length() == 0)
    		continue;
    	int x = stoi(num);
    	if(are_winning)
    		is_winning[x] = true;
    	else if (is_winning[x])
    		wins++;
    }
    return wins;
}

void wins_for_card(const string& card, int i) {
	smatch match;
	map<int, bool> is_winning{};
	regex_search(card, match, rcard);
	string winning_nums = match[1];
	string my_nums = match[2];
	const auto end = std::sregex_token_iterator();
    iterate_nums(is_winning, std::sregex_token_iterator(winning_nums.begin(), winning_nums.end(), split, -1), end, true);
    int x = iterate_nums(is_winning, std::sregex_token_iterator(my_nums.begin(), my_nums.end(), split, -1), end, false);
    for(int j = i + 1; j <= i + x; j++)
    	multipliers[j] += (multipliers[i]+1);
}

int sum_cards(ifstream& f) {
	string line;
	int sum = 0;
	int card = 0;
	while(!f.eof()) {
		getline(f, line);
		wins_for_card(line, card);
		sum += (multipliers[card]+1);
		card++;
	}
	return sum;
}

int main(){
	ifstream f("4.in");
    int sum = sum_cards(f);

	printf("%d\n", sum);
}