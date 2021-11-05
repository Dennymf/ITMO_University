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
const ll INF = 1e9;

int n, t[N];

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

	t[v] = min(t[v * 2 + 1], t[v * 2 + 2]);
}

ll min(int tl, int tr, int v, int l, int r)
{
	if (r < tl || l > tr)
	{
		return INF;
	}

	if (l >= tl && r <= tr)
	{
		return t[v];
	}

	int m = (l + r) / 2;

	return min(min(tl, tr, v * 2 + 1, l, m), min(tl, tr, v * 2 + 2, m + 1, r));
}

ll find_place(int l, int r)
{
	if (l == r)
	{
		return l;
	}

	int m = (l + r) / 2;

	if (min(l, r, 0, 0, n - 1) != 0)
	{
		return INF;
	}
	else
	{
		if (!min(l, m, 0, 0, n - 1))
		{
			find_place(l, m);
		}
		else
		{
			find_place(m + 1, r);
		}
	}
}

int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);
	
    freopen("parking.in", "r", stdin);
    freopen("parking.out", "w", stdout);
    
	int m;

	cin >> n >> m;

	for (int i = 0; i < m; i++)
	{
		string in;
		int ind;

		cin >> in >> ind;

		ind--;

		if (in == "enter")
		{
			int place = find_place(ind, n - 1);
			if (place == INF)
			{
				place = find_place(0, place);
			}
			cout << place + 1 << endl;

			update(place, 1, 0, 0, n - 1);
		}
		else
		{
			update(ind, 0, 0, 0, n - 1);
		}
	}
}