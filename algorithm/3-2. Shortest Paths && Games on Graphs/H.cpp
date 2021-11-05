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

void file()
{
    //freopen("game.in", "r", stdin);
   // freopen("game.out", "w", stdout);
}

vector<vector<int>> g, g1;
vector<bool> used, win, lose;
vector<int> topsort, deg;

void dfs_topsort(int v) {
    used[v] = true;

    for (int i = 0; i < g[v].size(); i++) {
        int to = g[v][i];

        if (!used[to]) {
            dfs_topsort(to);
        }
    }

    topsort.pb(v);
}

void top_sort() {
    for (int i = 0; i < g.size(); i++) {
        if (!used[i]) {
            dfs_topsort(i);
        }
    }

    reverse(topsort.begin(), topsort.end());
}

void dfs_color(int v) {
    used[v] = true;

    for (int i = 0; i < g1[v].size(); i++) {
        int to = g1[v][i];

        if (!used[to]) {
            if (lose[v]) {
                win[to] = true;
            }
            else if (--deg[to] == 0) {
                lose[to] = true;
            }
            else
            {
                continue;
            }

            dfs_color(to);
        }
    }
}

int main() {
    file();

    int n, m, s;

    cin >> n >> m >> s;
    s--;

    g.resize(n), g1.resize(n), used.resize(n),  win.resize(n), lose.resize(n), deg.resize(n);

    int x, y;

    for (int i = 0; i < m; i++) {
        cin >> x >> y;

        x--, y--;

        deg[x]++;
        g[x].pb(y);
        g1[y].pb(x);
    }


    top_sort();

    used.clear();
    used.resize(n);

    for (auto v : topsort) {
        if (g[v].size() == 0 && !used[v]) {
            lose[v] = true;
            dfs_color(v);
        }
    }


    if (win[s]) {
        cout << "First player wins";
    }
    else {
        cout << "Second player wins";
    }
}