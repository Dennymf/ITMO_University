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
	//freopen("input.txt", "r", stdin);
	//freopen("output.txt", "w", stdout);
}

vector<vector<pair<int, int>>> g, temp_g;
bool used[200000], bridg[200000];
int u[200000], tp[200000], t, color[200000], countColor;
vector<pair<int, int>> ed;
set<int> ans;

void dfs(int v, int p)
{
	used[v] = 1;
	t++;
	u[v] = tp[v] = t;

	for (int i = 0; i < g[v].size(); i++)
	{
		int to = g[v][i].x;

		if (to == p)
			continue;

		if (used[to])
			u[v] = min(tp[to], u[v]);
		else
		{
			dfs(to, v);
			u[v] = min(u[v], u[to]);
			if (u[to] > tp[v])
				bridg[g[v][i].y] = 1;
		}
	}
}

void dfs(int v)
{
	used[v] = 1;
	color[v] = countColor;

	for (int i = 0; i < g[v].size(); i++)
		if (!used[g[v][i].x])
			dfs(g[v][i].x);
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
		int x, y;
		cin >> x >> y;

		x--, y--;
		ed.emplace_back(x, y);
		g[x].pb({ y, i });
		g[y].pb({ x, i });
	}

	for (int i = 0; i < n; i++)
		if (!used[i])
			dfs(i, -1);

	for (int i = 0; i < n; i++)
		used[i] = 0;
	temp_g.resize(n);

	for (int i = 0; i < n; i++)
		for (int j = 0; j < g[i].size(); j++)
			if (!bridg[g[i][j].y])
			{
				temp_g[i].pb({ g[i][j].x, g[i][j].y });
				temp_g[g[i][j].x].pb({ i, g[i][j].y });
			}

	g = temp_g;

	for (int i = 0; i < n; i++)
		if (!used[i])
		{
			countColor++;
			dfs(i);
		}

	cout << countColor << endl;
	
	for (int i = 0; i < n; i++)
		cout << color[i] << " ";

	return 0;
}