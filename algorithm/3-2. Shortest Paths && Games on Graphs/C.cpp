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

vector<pair<pair<int, int>, int>> edg;
vector<int> dist, p;

void file()
{
    //freopen("avia.in", "r", stdin);
    //freopen("avia.out", "w", stdout);
}

int main()
{
    int n;

    cin >> n;

    int w;

    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
        {
            cin >> w;

            if (w != 100000)
                edg.pb({ {i, j}, w });
        }

    int m = edg.size();
    p.resize(n, -1);
    dist.resize(n, INF);

    int l;

    for (int i = 0; i < n; i++) {
        l = -1;
        for (auto edge : edg) {
            if (dist[edge.x.y] > dist[edge.x.x] + edge.y) {
                dist[edge.x.y] = max(-INF, (ll)(dist[edge.x.x] + edge.y));
                p[edge.x.y] = edge.x.x;
                l = edge.x.y;
            }
        }
    }


    if (l != -1) {
        int v = l;
        while (n--)
            v = p[v];

        vector<int> ans;

        int u = v;
        while (true) {
            if (ans.size() > 1 && u == v)
                break;
            ans.pb(u);
            u = p[u];
        }

        cout << "YES" << endl;
        cout << ans.size() << endl;
        for (auto it = ans.rbegin(); it != ans.rend(); it++) {
            cout << *it + 1 << " ";
        }
    }
    else {
        return cout << "NO", 0;
    }
}