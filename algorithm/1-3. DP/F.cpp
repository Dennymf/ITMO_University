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

ll n;
ll a[2000], dp[300][300];

signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

//	freopen("brackets2num.in", "r", stdin);
//	freopen("brackets2num.out", "w", stdout);

	cin >> n;

	for (int i = 0; i <= n + 2; i++)
		for (int j = 0; j <= n + 2; j++)
			dp[i][j] = INF;

	for (int i = 0; i < n; i++)
		cin >> a[i];

	dp[0][0] = 0;
	for(int i = 0; i < n; i++)
		for (int j = 0; j <= i; j++)
		{
			if (a[i] > 100)
				dp[i + 1][j + 1] = min(dp[i][j] + a[i], dp[i + 1][j + 1]);
			else
				dp[i + 1][j] = min(dp[i][j] + a[i], dp[i + 1][j]);

			if (j)
				dp[i + 1][j - 1] = min(dp[i + 1][j - 1], dp[i][j]);
		}

	ll ans = INF, i1 = 0;

	for (int i = 0; i <= n; i++)
		if (ans >= dp[n][i])
			ans = dp[n][i], i1 = i;

	ll notk = 0, k = 0;
	vector<ll> b;

	for (int i = n, j = i1; i > 0;)
	{
		if (!j)
		{
			if (dp[i][j] == dp[i - 1][j + 1])
				j++, k++, b.push_back(i), i--;
			else if (dp[i][j] == dp[i - 1][j] + a[i - 1])
				i--;
		}
		else
		{
			if (dp[i][j] == dp[i - 1][j + 1])
				j++, k++, b.push_back(i), i--;
			else if (dp[i][j] == dp[i - 1][j] + a[i - 1])
				i--;
			else if (dp[i][j] == dp[i - 1][j - 1] + a[i - 1])
				i--, j--, notk++;
		}
	}

	cout << ans << endl;
	cout << notk - k << ' ' << k << endl;
	reverse(b.begin(), b.end());
	for (int i = 0; i < b.size(); i++)
		cout << b[i] << endl;
}