#include <iostream>
#include <cmath>
#include <algorithm>
#include <cstring>
#include <cstdlib>
#include <cstdio>

#define x first
#define y second
#define sq(a) ((a) * (a))

using namespace std;

typedef long long ll;
typedef long double ld;
typedef unsigned long long ull;

const ld EPS = 1e-8;

pair<ld, ll> temp[200000];
ll ans[200000], v[200000], w[200000];
ll n, k;

bool comp(pair<ld, ll> a, pair<ld, ll> b)
{
	return a.x > b.x;
}
bool check(ld m)
{
	for (int i = 0; i < n; i++)
	{
		temp[i].x = m * w[i] - v[i];
		temp[i].y = i;
	}

	sort(temp, temp + n);

	ld ans = 0;

	for (int i = 0; i < k; i++)
		ans += temp[i].x;

	return (ans <= 0);
}
signed main()
{
	ios::sync_with_stdio();
	cin.tie();
	cout.tie();

	cin >> n >> k;

	for (int i = 0; i < n; i++)
		cin >> v[i] >> w[i];

	ld l = 0, r = 10000006;
	for(int i = 0; i < 100; i++)
	{
		ld m = (r + l) / 2;
		if (check(m))
		{
			l = m;
			for(int j = 0; j < k; j++)
				ans[j] = temp[j].y;
		}
		else
			r = m;
	}

	for (int i = 0; i < k; i++)
		cout << ans[i] + 1 << endl;
}