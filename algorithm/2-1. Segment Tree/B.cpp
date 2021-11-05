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

const ll N = 2e6 + 10;

ll a[N], t[N];

void build(int v, int l, int r)
{
	if (l == r)
	{
		t[v] = a[l];
		return;
	}

	int m = (l + r) / 2;

	build(v * 2 + 1, l, m);
	build(v * 2 + 2, m + 1, r);

	t[v] = t[v * 2 + 1] + t[v * 2 + 2];
}

void update(int i, int x, int v, int l, int r)
{
	if (l == r)
	{
		t[v] = x;
		return;
	}

	int m = (l + r) / 2;

	if (i <= m)
	{
		update(i, x, v * 2 + 1, l, m);
	}
	else
	{
		update(i, x, v * 2 + 2, m + 1, r);
	}
	t[v] = t[v * 2 + 1] + t[v * 2 + 2];
}

ll sum(int v, int l, int r, int tl, int tr)
{
	if (l >= tl && r <= tr)
	{
		return t[v];
	}
	if (r < tl || l > tr)
	{
		return 0;
	}

	int m = (l + r) / 2;

	return sum(v * 2 + 1, l, m, tl, tr) + sum(v * 2 + 2, m + 1, r, tl, tr);
}
int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);
	
	ll n;

	cin >> n;

	for (int i = 0; i < n; i++)
	{
		cin >> a[i];
	}

	build(0, 0, n - 1);

	string in;

	while (cin >> in)
	{
		if (in == "sum")
		{
			int l, r;

			cin >> l >> r;
			cout << sum(0, 0, n - 1, l - 1, r - 1) << '\n';
		}
		else
		{
			int i, x;

			cin >> i >> x;
			update(i - 1, x, 0, 0, n - 1);
		}
	}

}