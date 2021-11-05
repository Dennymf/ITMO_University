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

struct snm {
	ll p, maxx, minn, size;
};

snm a[2000000];
ll n;

ll find(int v)
{
	if (v == a[v].p)
		return v;
	return a[v].p = find(a[v].p);
}

void unionSet(int x, int y)
{
	x = find(x);
	y = find(y);
	if (x != y)
	{
		if (a[x].size < a[y].size)
			swap(x, y);
		a[y].p = x;
		a[x].minn = min(a[x].minn, a[y].minn);
		a[x].maxx = max(a[x].maxx, a[y].maxx);
		a[x].size += a[y].size;
	}
}
signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	cin >> n;
	for (int i = 1; i <= n; i++)
	{
		a[i].p = i;
		a[i].minn = i;
		a[i].maxx = i;
		a[i].size = 1;
	}

	string s;
	snm ans;
	ll v, x, y;
	while (cin >> s)
	{
		if (s == "get")
		{
			cin >> v;
			ans = a[find(v)];
			cout << ans.minn << ' ' << ans.maxx << ' ' << ans.size << '\n';
		}
		else
		{
			cin >> x >> y;
			unionSet(x, y);
		}
	}
}