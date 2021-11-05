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

ll n, m;
ll a[2000][2000], dp[2000][2000];

signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	freopen("input.txt", "r", stdin);
	freopen("output.txt", "w", stdout);

	cin >> n >> m;

	for (int i = 0; i < n; i++)
		for (int j = 0; j < m; j++)
			cin >> a[i][j];

	for (int i = 0; i < n; i++)
		for (int j = 0; j < m; j++)
			dp[i][j] = -INF;

	dp[0][0] = a[0][0];

	for(int i = 0; i < n; i++)
		for (int j = 0; j < m; j++)
		{
			if (i)
				dp[i][j] = max(dp[i - 1][j] + a[i][j], dp[i][j]);
			if (j)
				dp[i][j] = max(dp[i][j - 1] + a[i][j], dp[i][j]);
		}
	
	cout << dp[n - 1][m - 1] << endl;

	string ans = "";
	for(int i = n - 1, j = m - 1; j > 0 || i > 0;)
	{
		if(i && (dp[i][j] - a[i][j] == dp[i - 1][j]))
			i--, ans = 'D' + ans;
		else if (j && (dp[i][j] - a[i][j] == dp[i][j - 1]))
			j--, ans = 'R' + ans;
	}

	cout << ans;
}