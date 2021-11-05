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
	ll p,val;
};

snm a[2000000];
ll n, m;

ll find(int v)
{
	if (v == a[v].p)
		return v;
	return find(a[v].p);
}

void unionSet(int x, int y)
{
	x = find(x);
	y = find(y);
	if (x != y)
	{
		a[y].p = x;
		a[y].val -= a[x].val;
	}
}
ll get(int v)
{
	ll ans = a[v].val;
	if (v == a[v].p)
		return ans;
	return ans + get(a[v].p);
}

signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	cin >> n >> m;
	for (int i = 1; i <= n; i++)
	{
		a[i].p = i;
		a[i].val = 0;
	}

	string s;
	snm ans;
	ll v, x, y, val;
	for(int i = 0; i < m; i++)
	{
		cin >> s;
		if (s == "add")
		{
			cin >> v >> val;
			a[find(v)].val += val;
		}
		else if(s == "join")
		{
			cin >> x >> y;
			unionSet(x, y);
		}
		else
		{
			cin >> v;
			cout << get(v) << '\n';
		}
	}
}