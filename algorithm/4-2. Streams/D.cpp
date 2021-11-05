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

ll n, m;

struct Edge
{
    ll c, from, to, num, revNum;
    ll f;
    ll x, y;
    bool isReal, inner;

    Edge() = default;

    Edge(const Edge& other) = default;

    Edge& operator=(const Edge& other) = default;

    Edge(ll c, ll f, ll to, ll num) : c(c), f(f), to(to), num(num)
    {}
};

struct Cell
{
    Cell() = default;

    Cell(const Cell& other) = default;

    Cell& operator=(const Cell& other) = default;

    ll x, y;
    char ch;

    ll v1, v2;
    ll edgeIndex;
};

vector <Edge> edgesList;
vector <vector<ll>> g;
vector<bool> used;

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

const int dx[] = { -1, 1, 0, 0 };
const int dy[] = { 0, 0, 1, -1 };

bool check(ll x, ll y)
{
    return x >= 0 && x < n&& y >= 0 && y < m;
}

void addEdges(const Cell& c1, const Cell& c2, vector <Edge>& edgesList, vector <vector<ll>>& g) {
    Edge e{};
    e.c = INF;
    e.isReal = true;
    e.num = edgesList.size();
    e.from = c1.v2;
    e.to = c2.v1;
    e.f = 0;
    e.revNum = edgesList.size() + 1;
    e.x = c2.x;
    e.y = c2.y;
    edgesList.pb(e);

    Edge w{};
    w.c = 0;
    w.isReal = false;
    w.num = e.revNum;
    w.revNum = e.num;
    w.f = 0;
    w.from = e.to;
    w.to = e.from;
    w.x = c1.x;
    w.y = c1.y;
    edgesList.pb(w);

    g[c1.v2].pb(e.num);
    g[c2.v1].pb(w.num);
}

void addEdgesInNeighbors(int x, int y, const vector <vector<Cell>>& field, vector <Edge>& edgesList,
    vector <vector<ll>>& g) {
    if (field[x][y].ch == '#')
        return;

    for (int i = 0; i < 4; i++) {
        int x1 = x + dx[i];
        int y1 = y + dy[i];

        if (check(x1, y1) && field[x1][y1].ch != '#') {
            addEdges(field[x][y], field[x1][y1], edgesList, g);
        }
    }
}

void addV(vector <vector<ll>>& g) {
    g.push_back(vector<ll>());
}

void findMinCut(ll v, vector <vector<ll>>& g, vector<bool>& used, vector <Edge>& edgesList) {

    if (used[v]) {
        return;
    }

    used.at(v) = true;

    for (int i = 0; i < g.at(v).size(); i++) {
        auto e = edgesList.at(g.at(v).at(i));
        if (!used.at(e.to) && e.f < e.c) {
            findMinCut(e.to, g, used, edgesList);
        }
    }
}


int main(){
    pair <ll, ll> s = { -1, -1 };
    pair <ll, ll> t = { -1, -1 };

    cin >> n >> m;

    vector <vector<Cell>> field(n, vector<Cell>(m));

    size_t cnt = 0;
    for (size_t i = 0; i < n; i++) {
        string str;
        cin >> str;

        for (size_t j = 0; j < str.size(); j++){
            if (str.at(j) == '#') {
                Cell c{};
                c.x = i;
                c.y = j;
                c.ch = '#';
                field.at(i).at(j) = c;
                continue;
            }
            if (str.at(j) == 'A')
                s = { i, j };
            if (str.at(j) == 'B')
                t = { i, j };

            addV(g);
            Cell c{};
            c.x = i;
            c.y = j;
            c.ch = str.at(j);
            c.v1 = g.size() - 1;
            c.v2 = g.size();
            addV(g);
            c.edgeIndex = edgesList.size();
            field.at(i).at(j) = c;

            Edge e{};
            e.c = str.at(j) == '.' ? 1 : INF;
            e.f = 0;
            e.num = edgesList.size();
            e.isReal = true;
            e.from = c.v1;
            e.to = c.v2;
            e.revNum = e.num + 1;
            e.x = i;
            e.y = j;
            e.inner = true;
            edgesList.pb(e);

            Edge w{};
            w.c = 0;
            w.f = 0;
            w.num = e.revNum;
            w.revNum = e.num;
            w.from = e.to;
            w.to = e.from;
            w.isReal = false;
            w.x = i;
            w.y = j;
            w.inner = true;
            edgesList.pb(w);

            g[c.v1].pb(e.num);
            g[c.v2].pb(w.num);
        }
    }

    for (size_t i = 0; i < n; i++) {
        for (size_t j = 0; j < m; j++) {
            if (field[i][j].ch == '#') continue;

            addEdgesInNeighbors(i, j, field, edgesList, g);
        }
    }

    if (s.x == -1 || t.x == -1) {
        return cout << -1, 0;
    }

    ll startv = field[s.x][s.y].v1;
    ll finishv = field[t.x][t.y].v1;

    used.resize(g.size(), false);
    while (dfs(startv, INF + 100, g, used, finishv, edgesList) > 0)
    {
        used.clear();
        used.resize(g.size());
    }

    ll maxFlow = 0;

    for (auto& it : g[startv]) {
        auto e1 = edgesList[it];
        maxFlow += abs(e1.f);
    }

    if (maxFlow >= INF) {
        return cout << -1, 0;
    }

    cout << maxFlow << endl;

    used.clear();
    used.resize(g.size());

    findMinCut(startv, g, used, edgesList);

    for (int i = 0; i < edgesList.size(); i++) {
        if (edgesList[i].inner && edgesList[i].isReal && used[edgesList[i].from] ^ used[edgesList[i].to]) {
            field[edgesList[i].x][edgesList[i].y].ch = '+';
        }
    }

    for (const auto& row : field) {
        for (const auto& cell : row) {
            cout << cell.ch;
        }
        cout << endl;
    }
}