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

vector<vector<int>> g;
bool used[200000];
int u[200000], tp[200000], t, countBridg;
vector<pair<int, int>> ed;
map<pair<int, int>, bool> bridg;

void dfs(int v, int p)
{
	used[v] = 1;
	t++;
	u[v] = tp[v] = t;

	for (int to : g[v])
	{
		if (to == p)
			continue;

		if (used[to])
			u[v] = min(tp[to], u[v]);
		else
		{
			dfs(to, v);
			u[v] = min(u[v], u[to]);
			if (u[to] > tp[v])
			{
				bridg[{ to, v }] = bridg[{ v, to }] = 1;
				countBridg++;
			}
		}
	}
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
		g[x].pb(y);
		g[y].pb(x);
	}

	for (int i = 0; i < n; i++)
		if (!used[i])
			dfs(i, -1);

	cout << countBridg << endl;

	for (int i = 0; i < ed.size(); i++)
	{
		pair<int, int> e = ed[i];
		if (bridg[e])
			cout << i + 1 << endl;
	}

	return 0;
}