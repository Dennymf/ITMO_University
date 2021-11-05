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

vector<vector<int>> g, h;
bool used[200000];
vector<int> ans;
int color = 1, cmp[200000];
set<pair<int, int>> con;

void firstDfs(int v)
{
	used[v] = 1;

	for (int i = 0; i < g[v].size(); i++)
		if (!used[g[v][i]])
			firstDfs(g[v][i]);

	ans.pb(v);
}

void secondDfs(int v)
{
	cmp[v] = color;

	for (int i = 0; i < h[v].size(); i++)
		if (!cmp[h[v][i]])
			secondDfs(h[v][i]);
}
int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	int n, m, ans_count = 0;

	cin >> n >> m;
	
	g.resize(n);
	h.resize(n);

	for (int i = 0; i < m; i++)
	{
		int x, y;
		cin >> x >> y;

		x--, y--;
		g[x].pb(y);
		h[y].pb(x);
	}

	for (int i = 0; i < n; i++)
		if (!used[i])
			firstDfs(i);
	
	for (int i = ans.size() - 1; i >= 0; i--)
		if (!cmp[ans[i]])
			secondDfs(ans[i]), color++;
	
	for(int i = 0; i < n; i++)
		for(int j = 0; j < g[i].size(); j++)
			if (cmp[i] != cmp[g[i][j]] && !con.count({ min(cmp[i], cmp[g[i][j]]), max(cmp[i], cmp[g[i][j]]) }))
			{
				con.insert({ min(cmp[i], cmp[g[i][j]]), max(cmp[i], cmp[g[i][j]]) });
				ans_count++;
			}

	cout << ans_count;

	return 0;
}