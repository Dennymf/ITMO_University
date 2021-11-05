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
 
const ll N = 5 * 1e6 + 100;
const ll M = 1e4 + 100;
const ll INF = 1e18 + 123;
const ll MOD = 1e9 + 7;
 
using namespace std;
 
#define x first
#define y second
#define pb push_back

vector<vector<pair<int, int>>> g;
vector<ll> dist;
set<pair<ll, ll>> q;

void file()
{
	//freopen("avia.in", "r", stdin);
	//freopen("avia.out", "w", stdout);
}

int main()
{
	int n, m;
	
	cin >> n >> m;
	g.resize(n);
	dist.resize(n, INF);
	
	int x, y, z;
	 
	for (int i = 0; i < m; i++) {
        cin >> x >> y >> z;
        x--, y--;
		
        g[x].pb({y, z});
        g[y].pb({x, z});
    }

	dist[0] = 0;
    q.insert({0, 0});

    while (!q.empty()) {
        int v = (*q.begin()).y;
        q.erase(q.begin());

        for (auto t : g[v]) {
            int to = t.x;
            ll w = t.y;

            if (dist[v] + w < dist[to]) {
                q.erase({dist[to], to});
                dist[to] = dist[v] + w;
                q.insert({dist[to], to});
            }
        }
    }

    for (auto iter : dist) {
        cout << iter << " ";
    }
}