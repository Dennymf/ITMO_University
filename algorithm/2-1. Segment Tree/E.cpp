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

struct matrix
{
	int xx, xy, yx, yy;
};

const matrix E = { 1, 0, 0, 1 };

matrix a[N], t[N];
ll r, n, m;

matrix matrix_multiply(matrix a, matrix b)
{
	matrix c;

	c.xx = ((a.xx * b.xx) % r + (a.xy * b.yx) % r) % r;
	c.xy = ((a.xx * b.xy) % r + (a.xy * b.yy) % r) % r;
	c.yx = ((a.yx * b.xx) % r + (a.yy * b.yx) % r) % r;
	c.yy = ((a.yx * b.xy) % r + (a.yy * b.yy) % r) % r;

	return c;
}
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

	t[v] = matrix_multiply(t[v * 2 + 1], t[v * 2 + 2]);
}


void cout_matrix(matrix a)
{
	cout << a.xx << ' ' << a.xy << '\n' << a.yx << ' ' << a.yy << '\n' << '\n';
}

matrix query(int tl, int tr, int v, int l, int r)
{
	if (l > tr || r < tl)
	{
		return E;
	}
	if (l >= tl && r <= tr)
	{
		return t[v];
	}

	int m = (l + r) / 2;
	matrix left = query(tl, tr, v * 2 + 1, l, m);
	matrix right = query(tl, tr, v * 2 + 2, m + 1, r);

	return matrix_multiply(left, right);
}

int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	freopen("crypto.in", "r", stdin);
	freopen("crypto.out", "w", stdout);
	
	cin >> r >> n >> m;

	for (int i = 0; i < n; i++)
	{
		cin >> a[i].xx >> a[i].xy >> a[i].yx >> a[i].yy;
	}

	build(0, 0, n - 1);

	for (int i = 0; i < m; i++)
	{
		int l, r;

		cin >> l >> r;
		
		l--, r--;
		cout_matrix(query(l, r, 0, 0, n - 1));
	}
}