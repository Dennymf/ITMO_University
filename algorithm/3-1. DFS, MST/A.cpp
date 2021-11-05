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

vector<vector<int>> g;
int n, color[200000];
bool used[200000];
vector<int> ans;

bool cycle(int v)
{
	color[v] = 1;
	for(int i = 0; i < g[v].size(); i++)
	{ 
		if (color[g[v][i]] == 0) {
			if (cycle(g[v][i]))
				return true;
		}
		else if (color[g[v][i]] == 1) {
			return true;
		}
	}
	color[v] = 2;
	return false;
}
void dfs(int v)
{
	used[v] = 1;
	for (int i = 0; i < g[v].size(); i++)
		if (!used[g[v][i]])
			dfs(g[v][i]);

	ans.pb(v + 1);
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
		int x, y;

		cin >> x >> y;
		x--, y--;

		g[x].pb(y);
	}

	for (int i = 0; i < n; i++)
		if (!color[i] && cycle(i))
			return cout << -1, 0;

	for (int i = 0; i < n; i++)
		used[i] = 0;

	for (int i = 0; i < n; i++)
		if (!used[i])
			dfs(i);

	reverse(ans.begin(), ans.end());

	for (int i = 0; i < ans.size(); i++)
		cout << ans[i] << ' ';

	return 0;
}