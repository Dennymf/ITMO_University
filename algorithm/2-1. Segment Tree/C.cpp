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
const ll INF = 1e18;

struct node {
	ll val;
	ll dSet = INF;
	ll dAdd = 0;
};

ll a[N], b[N];
node t[N];

void push(int, int, int);

void build(int v, int l, int r)
{
	if (l == r)
	{
		t[v].val = a[l];
		return;
	}

	int m = (l + r) / 2;

	build(v * 2 + 1, l, m);
	build(v * 2 + 2, m + 1, r);

	t[v].val = min(t[v * 2 + 1].val, t[v * 2 + 2].val);
}

void update(int tl, int tr, int x, int v, int l, int r)
{
	push(v, l, r);

	if (l >= tl && r <= tr)
	{
		t[v].dAdd = 0;
		t[v].dSet = x;
		push(v, l, r);
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
	push(v, l, r);

	if (l >= tl && r <= tr)
	{
		return t[v].val;
	}
	if (r < tl || l > tr)
	{
		return INF;
	}

	int m = (l + r) / 2;

	return min(min(v * 2 + 1, l, m, tl, tr), min(v * 2 + 2, m + 1, r, tl, tr));
}

void add(int tl, int tr, ll x, int v, int l, int r)
{
	push(v, l, r);
	
	if (l >= tl && r <= tr)
	{
		t[v].dAdd += x;
		t[v].dSet = INF;
		push(v, l, r);
		return;
	}
	if (r < tl || l > tr)
	{
		return;
	}

	int m = (l + r) / 2;

	add(tl, tr, x, v * 2 + 1, l, m);
	add(tl, tr, x, v * 2 + 2, m + 1, r);

	t[v].val = min(t[v * 2 + 1].val, t[v * 2 + 2].val);
}

void push(int v, int l, int r)
{
	if (l == r)
	{
		if (t[v].dAdd)
		{
			t[v].val += t[v].dAdd, t[v].dAdd = 0;
		}
		else if (t[v].dSet != INF)
		{
			t[v].val = t[v].dSet, t[v].dSet = INF;
		}
	}
	if (t[v].dAdd) {
		if (t[v * 2 + 1].dSet != INF) {
			t[v * 2 + 1].dSet += t[v].dAdd, t[v * 2 + 1].dAdd = 0;
		}
		else if (t[v * 2 + 1].dAdd) {
			t[v * 2 + 1].dAdd += t[v].dAdd, t[v * 2 + 1].dSet = INF;
		}
		else if (!t[v * 2 + 1].dAdd && t[v * 2 + 1].dSet == INF) {
			t[v * 2 + 1].dAdd = t[v].dAdd, t[v * 2 + 1].dSet = INF;
		}

		if (t[v * 2 + 2].dSet != INF) {
			t[v * 2 + 2].dSet += t[v].dAdd, t[v * 2 + 2].dAdd = 0;
		}
		else if (t[v * 2 + 2].dAdd) {
			t[v * 2 + 2].dAdd += t[v].dAdd, t[v * 2 + 2].dSet = INF;
		}
		else if (!t[v * 2 + 2].dAdd && t[v * 2 + 2].dSet == INF) {
			t[v * 2 + 2].dAdd = t[v].dAdd, t[v * 2 + 2].dSet = INF;
		}

		t[v].val += t[v].dAdd;
		t[v].dAdd = 0;
		t[v].dSet = INF;
		return;
	}

	if (t[v].dSet != INF) {
		t[v * 2 + 1].dSet = t[v].dSet, t[v * 2 + 2].dSet = t[v].dSet;
		t[v * 2 + 1].dAdd = 0, t[v * 2 + 2].dAdd = 0;

		t[v].val = t[v].dSet;
		t[v].dSet = INF;
		t[v].dAdd = 0;
		return;
	}
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
		b[i] = a[i];
	}

	build(0, 0, n - 1);

	string in;

	while (cin >> in)
	{
		int l, r;

		cin >> l >> r;

		l--, r--;
		if (in == "min")
		{
			cout << min(0, 0, n - 1, l, r) << '\n';
		}
		else if(in == "set")
		{
			int x;

			cin >> x;
			update(l, r, x, 0, 0, n - 1);
		}
		else
		{
			ll x;
			
			cin >> x;
			add(l, r, x, 0, 0, n - 1);
		}
	}

}