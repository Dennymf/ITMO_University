#include <iostream>
#include <cmath>
#include <algorithm>
#include <iomanip>
#include <vector>
#include <set>
#include <map>
#include <deque>
#include <queue>
#include <stack>
#include <map>
#include <random>
#include <string>
#include <cstring>
#include <unordered_set>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 5 * 1e6 + 100;
const ll M = 1e4 + 100;
const int INF = 1e9 + 123;
const ll MOD = 1e9 + 7;

using namespace std;

#define x first
#define y second
#define pb push_back

int p[200000], r[200000];
vector<vector<pair<int, int>>> g;
vector<pair<int, pair<int, int>>> ed;
ll ans = 0;


int get_dsu(int v)
{
	if (v == p[v])
		return v;
	p[v] = get_dsu(p[v]);
	return p[v];
}

bool check_dsu(int v, int u)
{
	v = get_dsu(v);
	u = get_dsu(u);
	return u == v;
}

void union_dsu(int v, int u)
{
	v = get_dsu(v);
	u = get_dsu(u);

	if (u != v)
	{
		if (r[u] < r[v])
			swap(u, v);	
		p[v] = u;
		r[u] += r[v];
	}
}

void file()
{
	//freopen("avia.in", "r", stdin);
	//freopen("avia.out", "w", stdout);
}

int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	int n, m;

	cin >> n >> m;

	g.resize(n);

	for (int i = 0; i < m; i++)
	{
		int x, y, z;

		cin >> x >> y >> z;
		x--, y--;

		g[x].pb({ y, z });
		g[y].pb({ x, z });
		ed.pb({ z, { x, y } });
	}

	for (int i = 0; i < n; i++)
		p[i] = i, r[i] = 1;

	sort(ed.begin(), ed.end());

	for (int i = 0; i < m; i++)
	{
		pair<int, int> e = ed[i].y;
		if (!check_dsu(e.x, e.y))
		{
			union_dsu(e.x, e.y);
			ans += ed[i].x;
		}
	}

	cout << ans;

	return 0;
}