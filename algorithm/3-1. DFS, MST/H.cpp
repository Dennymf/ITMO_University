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


void file()
{
	freopen("avia.in", "r", stdin);
	freopen("avia.out", "w", stdout);
}

int color = 0, n;
vector<int> used;
vector<vector<pair<int, int>>> g, second_g;

void dfs(int v, int p, int m, vector<vector<pair<int, int>>>& g)
{
	used[v] = color;

	for (int i = 0; i < g[v].size(); i++)
		if (!used[g[v][i].x] && g[v][i].y <= m)
			dfs(g[v][i].x, v, m, g);
}

bool check(int m)
{
	color = 0;
	for (int i = 0; i < n; i++)
		used[i] = 0;

	for(int i = 0; i < n; i++)
		if (!used[i])
		{
			color++;
			dfs(i, -1, m, g);
		}

	int tmp = color;

	color = 0;
	for (int i = 0; i < n; i++)
		used[i] = 0;

	for (int i = 0; i < n; i++)
		if (!used[i])
		{
			color++;
			dfs(i, -1, m, second_g);
		}

	return color == 1 && tmp == 1;
}
int bin_search(int l, int r)
{
	while (r - l > 1)
	{
		int m = (l + r) / 2;

		if (check(m))
			r = m;
		else
			l = m;
	}
	return r;
}
int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);
	file();

	cin >> n;

	g.resize(n), used.resize(n), second_g.resize(n);

	for(int i = 0; i < n; i++)
		for (int j = 0; j < n; j++)
		{
			int x;

			cin >> x;

			g[i].pb({ j, x });
			second_g[j].pb({ i, x });
		}

	cout << bin_search(-1, 1000000500);

	return 0;
}