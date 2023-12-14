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

const regex rseeds { "^seeds: (.*)$" };
const regex space { "\\s+" };

vector<pair<long long, long long>> parse_seeds(string& input) {
	smatch match;
	vector<pair<long long, long long>> seeds;
	regex_match(input, match, rseeds);
	input = string(match[1]);
	const auto end = std::sregex_token_iterator();
	auto it = std::sregex_token_iterator(input.begin(), input.end(), space, -1);
	while(it != end) {
		seeds.push_back(make_pair(stoll(*it++), stoll(*it++)));
	}
	sort(seeds.begin(), seeds.end());
	return seeds;
}

long long min(long long a, long long b) {
	return a < b ? a : b;
}

map<long long, bool> changed;

pair<long long, long long> common_part(pair<long long, long long> p1, pair<long long, long long> p2) {
	if (p1.first > p2.first)
		swap(p1, p2);
	if (p1.first + p1.second <= p2.first)
		return make_pair(0, 0);
	return make_pair(p2.first, min(p2.second, p1.second - (p2.first - p1.first)));
}

void parse_map(ifstream& f, vector<pair<long long, long long>>& seeds) {
	string line;
	getline(f, line);
	int src, dst, range;
	vector<pair<long long, pair<long long, long long>>> ranges;
	vector<pair<long long, long long>> new_seeds{};

	while(f) {
		getline(f, line);
		if(line.length() <= 1 || f.eof())
			break;
		istringstream iss(line);
		iss >> dst >> src >> range;
		ranges.push_back(make_pair(dst, make_pair(src, range)));
	}
	for (auto& range : ranges) {
		int n = seeds.size();
		for (int i = 0; i < n; i++) {
			auto it = seeds.begin() + i; 
			auto& seed = seeds[i];
			auto p = common_part(range.second, seed);
			if (p.first == 0 && p.second == 0)
				continue;
			new_seeds.push_back(make_pair(p.first + range.first - range.second.first, p.second));
			if (seed.second + seed.first > p.first + p.second) {
				seed.second -= seed.first + seed.second - p.first - p.second;
				seeds.push_back(make_pair(p.first + p.second, seed.first + seed.second - p.first - p.second));
			}
			seed.second -= p.second;

		}
	}
	for (auto elem : seeds){
		if (elem.second > 0) {
			new_seeds.push_back(elem);
		}
	}
	seeds = std::move(new_seeds);
}

long long pipe_seeds(ifstream& f) {
	string line;
	getline(f, line);
	auto seeds = parse_seeds(line);
	getline(f, line);
	while(!f.eof()) {
		parse_map(f, seeds);
	}
	long long min_loc = seeds[0].first;
	for(auto seed: seeds) {
		min_loc = min(min_loc, seed.first);
	}
	return min_loc;
}

int main(){
	ifstream f("5t.in");

    long long sum = pipe_seeds(f);
	printf("%lld\n", sum);
}