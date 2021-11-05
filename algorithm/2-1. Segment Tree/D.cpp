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
const ll N = 4e6 + 10;
const ll INF = 1e18;
struct node {
	int val = 0, d = -1, ans = 0, rbit = 0, lbit = 0;
};
struct query {
	int col, l, r;
};
int n, maxr;
node t[N];
query q[N];
void push(int v)
{
	if (t[v].d == -1)
	{
		return;
	}

	t[v * 2 + 1].d = t[v * 2 + 2].d = t[v].d;
	t[v].d = -1;
}
int tval(int v, int l, int r)
{
	if (t[v].d == 0)
	{
		return 0;
	}
	else if (t[v].d == 1)
	{
		return r - l + 1;
	}
	else
		return t[v].val;
}
int tlbit(int v)
{
	if (t[v].d != -1)
	{
		return t[v].d;
	}

	return t[v].lbit;
}
int tans(int v)
{
	if (t[v].d != -1)
	{
		return t[v].d;
	}
	return t[v].ans;
}
int trbit(int v) {
	if (t[v].d != -1)
	{
		return t[v].d;
	}
	return t[v].rbit;
}
void update(int tl, int tr, int x, int v, int l, int r)
{
	if (l >= tl && r <= tr)
	{
		t[v].d = x;
		return;
	}
	if (r < tl || l > tr) {
		return;
	}
	int m = (l + r) / 2;
	push(v);
	update(tl, tr, x, v * 2 + 1, l, m);
	update(tl, tr, x, v * 2 + 2, m + 1, r);
	t[v].val = tval(v * 2 + 1, l, m) + tval(v * 2 + 2, m + 1, r);
	t[v].lbit = tlbit(v * 2 + 1);
	t[v].rbit = trbit(v * 2 + 2);
	if (trbit(v * 2 + 1) == tlbit(v * 2 + 2) && trbit(v * 2 + 1) == 1)
	{
		t[v].ans = tans(v * 2 + 1) + tans(v * 2 + 2) - 1;
	}
	else
	{
		t[v].ans = tans(v * 2 + 1) + tans(v * 2 + 2);
	}
}
int find_pow(int x) {
	int p = 1;
	while (p < x) {
		p *= 2;
	}
	return p * 2;
}
int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	cin >> n;
	maxr = -1;
	int minneg = 0;
	for (int i = 0; i < n; i++)
	{
		char c;
		int left, len;
		cin >> c >> left >> len;

		q[i].col = (c == 'W') ? 0 : 1;
		q[i].l = left;
		q[i].r = left + len - 1;

		minneg = min(minneg, q[i].l);
		maxr = max(maxr, q[i].r);
	}
	if (minneg < 0)
	{
		for (int i = 0; i < n; i++)
		{
			q[i].l -= minneg, q[i].r -= minneg;
			maxr = max(maxr, q[i].r);
		}
	}
	int size = find_pow(maxr);
	for (int i = 0; i < n; i++)
	{
		update(q[i].l, q[i].r, q[i].col, 0, 0, size - 1);
		cout << t[0].ans << " " << t[0].val << endl;
	}
}