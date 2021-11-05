#include <iostream>
#include <cmath>
#include <algorithm>
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <fstream>
#include <iomanip>
#include <map>
#include <unordered_map>
#include <set>
#include <unordered_set>
#include <vector>
#include <queue>
#include <deque>
#include <stack>
#include <ctime>
#include <string>
#include <vector>

#define x first
#define y second
#define sq(a) ((a) * (a))

using namespace std;

typedef long long ll;
typedef long double ld;
typedef unsigned long long ull;

const ll INF = 1e10;

string s, s1;
ll d[4000][4000];

signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

//	freopen("input.txt", "r", stdin);
//	freopen("output.txt", "w", stdout);

	cin >> s >> s1;

	d[0][0] = 0;
	for (int i = 1; i <= s.size(); i++)
		d[i][0] = i;
	for (int j = 1; j <= s1.size(); j++)
		d[0][j] = j;

	for(int i = 1; i <= s.size(); i++)
		for (int j = 1; j <= s1.size(); j++)
			d[i][j] = min(min(d[i - 1][j] + 1, d[i][j - 1] + 1), d[i - 1][j - 1] + (s[i - 1] == s1[j - 1] ? 0 : 1));

	cout << d[s.size()][s1.size()];
}