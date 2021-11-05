#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <algorithm>

using namespace std;

#define x first
#define y second
#define sq(a) ((a) * (a))

typedef long long ll;
typedef long double ld;
typedef unsigned long ul;

const ll N = 1e6 + 10;
const ll INF = 1e18;

ll a[N], logs[N], t[N][25];

ll function(ll x)
{
	if (x == 1)
	{
		return 0;
	}

	return function(x / 2) + 1;
}

ll query_min(ll l, ll r)
{
	if (l == r)
	{
		return a[l];
	}

	ll dist = r - l + 1;

	return min(t[l][logs[dist]], t[r - (1 << logs[dist]) + 1][logs[dist]]);
}

int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	ll n, m, v, u;

	cin >> n >> m >> a[0] >> u >> v;

	for (ll i = 1; i < n; i++)
	{
		a[i] = (23 * a[i - 1] + 21563) % 16714589;
		logs[i] = function(i);
	}

	for (ll i = 0; i < N; i++)
	{
		for (int j = 0; j < 20; j++)
		{
			t[i][j] = INF;
		}
	}

	for (ll i = 0; i < n; i++)
	{
		t[i][0] = a[i];
	}

	ll pow = function(n) + 2;

	for (ll j = 1; j < pow; j++)
	{
		for (ll i = 0; i < n; i++)
		{
			t[i][j] = min(t[i][j - 1], t[i + (1 << (j - 1))][j - 1]);
		}
	}

	ll ans = 0;

	for (ll i = 1; i <= m; i++)
	{
		if (v > u)
		{
			ans = query_min(u - 1, v - 1);
		}
		else
		{
			ans = query_min(v - 1, u - 1);
		}
		if (i < m)
		{
			u = ((17 * u + 751 + ans + 2 * i) % n + 1);
			v = ((13 * v + 593 + ans + 5 * i) % n + 1);
		}
	}

	cout << u << ' ' << v << ' ' << ans;
}