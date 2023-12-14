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


enum Type { HIGH_CARD, ONE_PAIR, TWO_PAIR, THREE, HOUSE, FOUR, FIVE };

string printEnum(Type t) {
	switch(t) {
	case 0:
		return "HIGH_CARD";
		break;
	case 1:
		return "ONE_PAIR";
		break;
	case 2:
		return "TWO_PAIR,";
		break;
	case 3:
		return "THREE";
		break;
	case 4:
		return "HOUSE";
		break;
	case 5:
		return "FOUR";
		break;
	case 6:
		return "FIVE";
		break;
	}
}

struct HandRange {
	string::const_iterator begin, end;
	HandRange(string::const_iterator b, string::const_iterator e) : begin(b), end(e) {}
};

class Hand {
private:
	int bet;
	string hand;
	Type type;
public:
	static inline map<char, int> order = {
		{'T', 0},
		{'J', 1},
		{'Q', 2},
		{'K', 3},
		{'A', 4}
	};
	Hand(string h, int b) {
		hand = h;
		bet = b;
		sort(h.begin(), h.end());
		vector<int> groups;
		for (int i = 0; i < h.length(); i++) {
			if (h[i] == 'J')
				continue;
			int reps = 1;
			while (i+1 < h.length() && h[i] == h[i+1]) {
				reps++, i++;
			}
			groups.push_back(reps);
		}
		sort(groups.begin(), groups.end());
		switch (groups.size()) {
			case 5:
				type = Type::HIGH_CARD;
				break;
			case 4:
				type = Type::ONE_PAIR;
				break;
			case 3:
				if (groups[0] + groups[1] == 2)
					type = Type::THREE;
				else
					type = Type::TWO_PAIR;
				break;
			case 2:
				if (groups[0] == 1)
					type = Type::FOUR;
				else
					type = Type::HOUSE;
				break;
			case 1:
			case 0:
				type = Type::FIVE;
				break;
		}
	}
	int getBet() const {
		return bet;
	}
	HandRange getRange() const {
		return HandRange(hand.begin(), hand.end());
	}
	Type getType() const {
		return type;
	}
	const string& getHand() const {
		return hand;
	}
};

char swap_joker(char c) {
	if (c == 'J')
		return '1';
	return c;
}

bool is_smaller(char c1, char c2) {
	c1 = swap_joker(c1), c2 = swap_joker(c2);
	if (c1 < 'A' || c2 < 'A')
		return c1 < c2;
	return Hand::order[c1] < Hand::order[c2];
}

bool operator<(const Hand& h1, const Hand& h2) {
	if (h1.getType() == h2.getType()) {
		auto hr1 = h1.getRange(), hr2 = h2.getRange();
		while(hr1.begin != hr1.end && hr2.begin != hr2.end) {
			if (*hr1.begin != *hr2.begin)
				return is_smaller(*hr1.begin, *hr2.begin);
			hr1.begin++, hr2.begin++;
		}
		return false;
	}
	return h1.getType() < h2.getType();
}


long long read_input(ifstream& f) {
	string s;
	long long sum = 0;
	vector<Hand> hands;
	while(!f.eof()) {
		getline(f, s);
		hands.push_back(Hand(s.substr(0, 5), stoi(s.substr(6, s.length() - 6))));
	}
	sort(hands.begin(), hands.end());
	for(int i = 0; i < hands.size(); i++)
		sum += hands[i].getBet() * (i+1);
	return sum;
}


int main(){
	ifstream f("7.in");
	printf("%lld\n", read_input(f));
	return 0;
}