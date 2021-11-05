#include <iostream>
#include <iomanip>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <vector>
#include <map>
#include <unordered_set>
#include <string>
#include <queue>
#include <set>

#define x first
#define y second
#define sq(a) ((a) * (a))
#define pb push_back

using namespace std;

typedef long long ll;
typedef long double ld;

const ll INF = 1e9 + 100;

struct Edge
{
    ll c, from, to, num, revNum;
    ll f;
    bool isReal;

    Edge() = default;

    Edge(const Edge& other) = default;

    Edge& operator=(const Edge& other) = default;

    Edge(ll c, ll f, ll to, ll num) : c(c), f(f), to(to), num(num)
    {}
};

ll dfs(ll v, ll minC, vector<vector<ll>>& g, vector<bool>& used, ll t, vector<Edge>& edgesList) {
    if (v == t) {
        return minC;
    }

    used[v] = true;

    for (int i = 0; i < g[v].size(); i++) {
        Edge e = edgesList[g[v][i]];
        if (!used[e.to] && e.f < e.c) {
            ll delta = dfs(e.to, min(minC, e.c - e.f), g, used, t, edgesList);
            if (delta > 0) {
                edgesList[g[v][i]].f += delta;
                edgesList[edgesList[g[v][i]].revNum].f -= delta;
                return delta;
            }
        }
    }

    return 0;
}

bool pathFound = false;
void findPath(ll v, ll t, vector<vector<ll>>& g, vector<Edge>& edgesList, vector<ll>& path) {
    if (v == t) {
        pathFound = true;
        return;
    }


    for (int i = 0; i < g[v].size(); i++) {
        Edge e = edgesList[g[v][i]];
        if (!e.isReal)
            continue;
        if (!pathFound && e.f == 1) {
            edgesList[g[v][i]].f = 0;
            path.pb(e.to);
            findPath(e.to, t, g, edgesList, path);
        }
    }
}

int main(){
    int n, m, s, t;
    cin >> n >> m >> s >> t;
    s--, t--;

    vector<vector<ll>> g(n);
    vector<Edge> edgesList;

    for (int i = 0; i < m; i++) {
        ll u, v;
        cin >> u >> v;
        u--, v--;

        if (u == v)
            continue;

        Edge e = Edge(1, 0, v, edgesList.size());
        e.revNum = edgesList.size() + 1;
        e.from = u;
        e.isReal = true;
        edgesList.pb(e);

        g[u].pb(e.num);

        Edge w = Edge(0, 0, u, edgesList.size());
        w.revNum = e.num;
        w.from = v;
        edgesList.pb(w);

        g[v].pb(w.num);
    }

    vector<bool> used(n);

    while (dfs(s, INF, g, used, t, edgesList) > 0) {
        used.clear();
        used.resize(n);
    }

    ll maxFlow = 0;

    for (const auto it : edgesList){
        if (it.isReal && it.from == s){
            maxFlow += it.f;
        }
    }

    if (maxFlow < 2) {
        return cout << "NO", 0;
    }

    vector<ll> p1, p2;
    p1.pb(s);
    p2.pb(s);

    findPath(s, t, g, edgesList, p1);
    pathFound = false;
    findPath(s, t, g, edgesList, p2);

    if (p1.back() != t || p2.back() != t) {
        return cout << "NO", 0;
    }

    cout << "YES" << '\n';

    for (const ll it : p1) {
        cout << it + 1 << " ";
    }

    cout << '\n';

    for (const ll it : p2) {
        cout << it + 1 << " ";
    }

    cout << '\n';
}