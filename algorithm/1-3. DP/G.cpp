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

string s;
ll dp[400][400], n;

bool check(char l, char r)
{
	if (l == '(' && r == ')')
		return 1;
	else if (l == '[' && r == ']')
		return 1;
	else if (l == '{' && r == '}')
		return 1;
	else
		return 0;
}

void out(int l, int r)
{
	if (dp[l][r] == 1 + dp[l + 1][r])
	{
		out(l + 1, r);
		return;
	}
	for(int j = l + 1; j <= r; j++)
		if (check(s[l], s[j]))
			if (dp[l][r] == dp[l + 1][j - 1] + dp[j + 1][r])
			{
				cout << s[l];
				out(l + 1, j - 1);
				cout << s[j];
				out(j + 1, r);
				return;
			}
	return;
}
signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	//	freopen("brackets2num.in", "r", stdin);
	//	freopen("brackets2num.out", "w", stdout);

	cin >> s;

	n = s.size();
	for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++)
			if (i == j)
				dp[i][j] = 1;
			else if (j < i)
				dp[i][j] = 0;
			else
				dp[i][j] = INF;

	for(int l = n - 1; l >= 0; l--)
		for(int r = l + 1; r < n; r++)
		{
			dp[l][r] = min(dp[l][r], dp[l + 1][r] + 1);
			for (int j = l + 1; j <= r; j++)
				if (check(s[l], s[j]))
					dp[l][r] = min(dp[l][r], dp[l + 1][j - 1] + dp[j + 1][r]);
		}
	
	//cout << dp[0][n - 1];
	out(0, n - 1);
}