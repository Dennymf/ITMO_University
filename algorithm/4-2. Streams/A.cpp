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

typedef long long ll;
typedef long double ld;

using namespace std;

const ll N = 1e6 + 10;
const ll INF = 1e9 + 100;
const ll NO_EDGE = 100000;
const ll M = 6e3 + 100;

#define x first
#define y second
#define pb push_back

struct Edge
{
    ll c, from, to, num, revNum;
    ll f;

    Edge() = default;

    Edge(const Edge& other) = default;

    Edge& operator=(const Edge& other) = default;

    Edge(ll c, ll f, ll to, ll num) : c(c), f(f), to(to), num(num)
    {}
};


ll dfs(ll v, ll minC, vector<vector<ll>>& g, vector<bool>& used, ll t, vector<Edge>& edgesList)
{
    if (v == t)
    {
        return minC;
    }

    used[v] = true;


    for (int i = 0; i < g[v].size(); i++)
    {
        Edge e = edgesList[g[v][i]];
        if (!used[e.to] && e.f < e.c)
        {
            ll delta = dfs(e.to, min(minC, e.c - e.f), g, used, t, edgesList);
            if (delta > 0)
            {
                edgesList[g[v][i]].f += delta;
                edgesList[edgesList[g[v][i]].revNum].f -= delta;
                return delta;
            }
        }
    }

    return 0;
}


int main()
{

    int n, m;
    cin >> n >> m;

    vector<pair<ll, ll>> edgeNums(m);
    vector<vector<ll>> g(n);
    vector<Edge> edgesList;

    for (int i = 0; i < m; i++)
    {
        ll u, v, c;
        cin >> u >> v >> c;

        u--, v--;

        Edge e = Edge(c, 0, v, edgesList.size());
        e.revNum = edgesList.size() + 1;
        e.from = u;
        edgesList.pb(e);

        g[u].pb(e.num);

        Edge w = Edge(c, 0, u, edgesList.size());
        w.revNum = e.num;
        w.from = v;
        edgesList.pb(w);

        g[v].pb(w.num);

        edgeNums[i] = { u, g[u].size() - 1 };
    }

    const int s = 0, t = n - 1;

    vector<bool> used(n);

    while (dfs(s, INF, g, used, t, edgesList) > 0)
    {
        used.clear();
        used.resize(n);
    }

    ll maxFlow = 0;

    for (auto it : g[0])
    {
        auto e1 = edgesList[it];
        maxFlow += abs(e1.f);
    }

    cout << maxFlow << endl;

    for (int i = 0; i < edgesList.size(); i += 2)
    {
        cout << (edgesList[i].f == 0 ? -edgesList[i + 1].f : edgesList[i].f) << endl;
    }

}