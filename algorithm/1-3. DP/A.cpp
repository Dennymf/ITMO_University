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

ll n, k;
ll a[200000], dp[200000];
vector<int> pt;

signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	freopen("input.txt", "r", stdin);
	freopen("output.txt", "w", stdout);

	cin >> n >> k;
	for (int i = 1; i < n - 1; i++)
	{
		cin >> a[i];
	}

	dp[0] = 0;
	for (int i = 1; i < n; i++)
		dp[i] = -1000000000;

	for (int i = 0; i < n; i++)
		for (int j = max(0ll, i - k); j < i; j++)
			dp[i] = max(dp[i], dp[j] + a[i]);

	cout << dp[n - 1] << endl;
	pt.push_back(n);
	for (int i = n - 1; i > 0;)
	{
		for (int j = max(0ll, i - k); j < i; j++)
			if (dp[i] - a[i] == dp[j])
				pt.push_back(j + 1), i = j;
	}
	cout << pt.size() - 1 << endl;
	reverse(pt.begin(), pt.end());
	for (int i = 0; i < pt.size(); i++)
		cout << pt[i] << ' ';
}