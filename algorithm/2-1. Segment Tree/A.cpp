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

const ll N = 1e7 + 10;

int a[N], c[N * 2];
ll sum[N];

int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);
	
	ll n, x, y;

	cin >> n >> x >> y >> a[0];

	for (int i = 1; i < n; i++)
	{
		a[i] = ((x * a[i - 1]) % (1 << 16) + y) % (1 << 16);
		sum[i] = sum[i - 1] + a[i];
	}

	ll m, z, t, b;

	cin >> m >> z >> t >> b;
    
	if (b < 0)
	{
		c[0] = ((1 << 30) + b) % n;
	}
	else
	{
		c[0] = (b % (1 << 30)) % n;
	}

	for (int i = 1; i < 2 * m; i++)
	{
		b = (z * b % (1 << 30)) + t;
		if (b < 0)
		{
			c[i] = ((1 << 30) + b) % n;
		}
		else
		{
			c[i] = (b % (1 << 30)) % n;
		}
	}

	ll ans = 0;

	ll l, r;	

	for (int i = 0; i < m; i++)
	{
		l = min(c[i * 2], c[i * 2 + 1]);
		r = max(c[i * 2], c[i * 2 + 1]);

		ans += sum[r] - sum[l] + a[l];
	}

	cout << ans;

}