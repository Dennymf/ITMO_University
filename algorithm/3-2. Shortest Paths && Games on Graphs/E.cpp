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

vector<pair<pair<ll, ll>, ll>> edg;
vector<ll> dist;
vector<bool> used;
vector<vector<ll>> g;

void dfs(ll v) {
    used[v] = true;

    for (auto to : g[v]) {
        if (!used[to]) {
            dfs(to);
        }
    }
}

void file()
{
    //freopen("avia.in", "r", stdin);
    //freopen("avia.out", "w", stdout);
}

int main() {
    ll n, m, s;
    cin >> n >> m >> s;
    s--;

    used.resize(n);
    g.resize(n);

    for (int i = 0; i < m; i++) {
        ll x, y, z;
        cin >> x >> y >> z;
        x--, y--;

        g[x].pb(y);

        edg.pb({ {x, y}, z });
    }


    dist.resize(n, INF);

    dist[s] = 0;

    set<ll> noPath;
    for (int i = 0; i < n; i++) {
        for (auto edge : edg) {
            if (dist[edge.x.x] != INF) {
                if (dist[edge.x.y] > dist[edge.x.x] + edge.y) {
                    dist[edge.x.y] = dist[edge.x.x] + edge.y;
                    if (i == n - 1) {
                        noPath.insert(edge.x.y);
                    }
                }
            }
        }
    }

    for (auto v : noPath) {
        if (!used[v]) {
            dfs(v);
        }
    }

    for (int i = 0; i < n; i++) {
        if (used[i]) {
            cout << '-' << endl;
        }
        else if (dist[i] == INF) {
            cout << '*' << endl;
        }
        else {
            cout << dist[i] << endl;
        }
    }
}