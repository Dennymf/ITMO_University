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
vector<ll> c;

bool check(int prev, int cur)
{
	int color = -1;

	for (int i = 0; i < m; i++, prev /= 2, cur /= 2)
	{
		int curb = cur % 2, prevb = prev % 2;
		
		if (curb == prevb)
		{
			if (curb == color)
				return 0;
			color = curb;
		}
		else
			color = -1;
	}
	return 1;
}

signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

//	freopen("brackets2num.in", "r", stdin);
//	freopen("brackets2num.out", "w", stdout);

	cin >> n >> m;

	if (m > n)
		swap(n, m);

	if (m == 0)
		return cout << 0, 0;

	int k = (1 << m);

	for (int i = 1; i <= k; i++)
		c.push_back(1);

	for (int i = 2; i <= n; i++)
	{
		vector<ll> nc;
		for (int j = 0; j <= k; j++)
			nc.push_back(0);
		for (int prev = 0; prev < k; prev++)
		{
			for (int cur = 0; cur < k; cur++)
			{
				if (check(prev, cur))
					nc[cur] += c[prev];
			}
		}
		c = nc;
	}

	ll ans = 0;

	for (int i = 0; i < c.size(); i++)
		ans += c[i];

	cout << ans;
}