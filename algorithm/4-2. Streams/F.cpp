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
#include <unordered_map>

#define x first
#define y second
#define sq(a) ((a) * (a))

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

    Edge revEdge()
    {
        Edge rev{};
        rev.f = 0;
        rev.revNum = num;
        rev.num = revNum;
        rev.c = 0;
        rev.from = to;
        rev.to = from;

        return rev;
    }

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

void findMinCut(ll v, vector<vector<ll>>& g, vector<bool>& used, vector<Edge>& edgesList){
    if (used[v]) {
        return;
    }

    used[v] = true;

    for (int i = 0; i < g[v].size(); i++) {
        auto e = edgesList[g[v][i]];

        if (!used[e.to] && e.f < e.c) {
            findMinCut(e.to, g, used, edgesList);
        }
    }
}

int main(){
    int n;
    cin >> n;
    int s = 0;
    vector<ll> scoredPoints(n, 0);
    vector<vector<ll>> g(n + 1);
    vector<Edge> edgesList;
    unordered_map<char, int> symbol2Score;
    char score2Symbol[4];
    unordered_map<int, pair<int, int>> games;
    vector<vector<char>> table(n, vector<char>(n));

    symbol2Score['W'] = 3;
    symbol2Score['w'] = 2;
    symbol2Score['l'] = 1;
    symbol2Score['L'] = 0;
    symbol2Score['.'] = 0;

    score2Symbol[3] = 'W';
    score2Symbol[2] = 'w';
    score2Symbol[1] = 'l';
    score2Symbol[0] = 'L';

    for (int i = 0; i < n; i++) {
        string str;
        cin >> str;

        for (int j = 0; j < str.size(); j++) {
            char result = str[j];

            table[i][j] = str[j];

            if (i == j)
                continue;

            scoredPoints[i] += symbol2Score[result];

            if (j < i && result == '.') {
                g.push_back(vector<ll>());

                size_t gameIndex = g.size() - 1;

                Edge e{};
                e.from = s;
                e.to = gameIndex;
                e.c = 3;
                e.num = edgesList.size();
                e.revNum = e.num + 1;
                e.f = 0;
                Edge revE = e.revEdge();

                edgesList.push_back(e);
                edgesList.push_back(revE);

                g[e.from].push_back(e.num);
                g[revE.from].push_back(revE.num);

                pair<int, int> opponents = { i + 1, j + 1 };
                games[gameIndex] = opponents;

                Edge w{};
                w.from = gameIndex;
                w.to = opponents.x;
                w.f = 0;
                w.c = 3;
                w.num = edgesList.size();
                w.revNum = w.num + 1;
                Edge revW = w.revEdge();

                edgesList.push_back(w);
                edgesList.push_back(revW);
                g[w.from].push_back(w.num);
                g[revW.from].push_back(revW.num);

                Edge z{};
                z.from = gameIndex;
                z.to = opponents.y;
                z.f = 0;
                z.c = 3;
                z.num = edgesList.size();
                z.revNum = z.num + 1;
                Edge revZ = z.revEdge();

                edgesList.push_back(z);
                edgesList.push_back(revZ);
                g[z.from].push_back(z.num);
                g[revZ.from].push_back(revZ.num);
            }
        }
    }

    vector<ll> needPoints(n, 0);

    for (int i = 0, x; i < n; i++) {
        cin >> x;
        needPoints[i] = x - scoredPoints[i];
    }

    const int t = g.size();
    g.push_back(vector<ll>());

    for (int i = 1; i <= n; i++) {
        Edge e{};
        e.from = i;
        e.to = t;
        e.c = needPoints[i - 1];
        e.f = 0;
        e.num = edgesList.size();
        e.revNum = e.num + 1;
        Edge revE = e.revEdge();
        edgesList.push_back(e);
        edgesList.push_back(revE);
        g[i].push_back(e.num);
        g[t].push_back(revE.num);
    }

    vector<bool> used(g.size(), false);
    while (dfs(s, INF, g, used, t, edgesList) > 0) {
        used.clear();
        used.resize(g.size());
    }

    for (const auto& it : games) {
        int gameIndex = it.x;
        vector<ll> edgeNumbers = g[gameIndex];
        vector<ll> realEdges;

        std::copy_if(
            edgeNumbers.begin(),
            edgeNumbers.end(),
            std::back_inserter(realEdges),
            [&edgeNumbers, &edgesList](ll i) {return edgesList[i].c == 3; });


        auto e = edgesList[realEdges[0]];
        auto w = edgesList[realEdges[1]];
        ll opp1 = e.to, f1 = e.f;
        ll opp2 = w.to, f2 = w.f;

        table[opp1 - 1][opp2 - 1] = score2Symbol[e.f];
        table[opp2 - 1][opp1 - 1] = score2Symbol[w.f];
    }

    for (const auto& row : table) {
        for (const auto c : row) {
            cout << c;
        }
        cout << endl;
    }
}