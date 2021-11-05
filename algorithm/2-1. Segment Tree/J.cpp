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

const ll N = 5e6 + 10;
const ll INF = 1e18;

struct node {
	ll val, d = -INF;
};

int n;
node t[N];

void push(int v) {
	if (t[v].d == -INF) {
		return;
	}
	if (t[v * 2 + 1].d < t[v].d)
	{
		t[v * 2 + 1].d = t[v].d;
	}
	if (t[v * 2 + 2].d < t[v].d)
	{
		t[v * 2 + 2].d = t[v].d;
	}
	if (t[v].d > t[v].val)
	{
		t[v].val = t[v].d;
	}
	t[v].d = -INF;
}

void update(int tl, int tr, int x, int v, int l, int r)
{
	push(v);

	if (l >= tl && r <= tr)
	{
		if (x > t[v].val)
		{
			t[v].d = x;
		}
		push(v);
		return;
	}

	if (r < tl || l > tr)
	{
		return;
	}
	
	int m = (l + r) / 2;

	update(tl, tr, x, v * 2 + 1, l, m);
	update(tl, tr, x, v * 2 + 2, m + 1, r);

	t[v].val = min(t[v * 2 + 1].val, t[v * 2 + 2].val);
}

ll min(int v, int l, int r, int tl, int tr)
{
	push(v);
	if (l >= tl && r <= tr)
	{
		return t[v].val;
	}

	if (r < tl || l > tr)
	{
		return INF;
	}

	push(v);

	int m = (l + r) / 2;

	return min(min(v * 2 + 1, l, m, tl, tr), min(v * 2 + 2, m + 1, r, tl, tr));
}

ll find_min(int l, int r, int val)
{
	if (l == r)
	{
		return l;
	}

	int m = (l + r) / 2;

	if (min(0, 0, n - 1, l, m) == val)
	{
		find_min(l, m, val);
	}
	else
	{
		find_min(m + 1, r, val);
	}
}

int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	int m;

	cin >> n >> m;

	for (int i = 0; i < m; i++)
	{
		string in;
		int l,  r;

		cin >> in;
		cin >> l >> r;
		l--, r--;
		if (in == "defend")
		{
			ll x;
			cin >> x;
			update(l, r, x, 0, 0, n - 1);
		}
		else
		{
			ll ans = min(0, 0, n - 1, l, r);
			cout << ans << ' ' << find_min(l, r, ans) + 1 << endl;
		}
	}
}