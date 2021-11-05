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
vector<ll> dist;

void file()
{
    //freopen("avia.in", "r", stdin);
    //freopen("avia.out", "w", stdout);
}

int main() {
    ll n, m, k, s;

    cin >> n >> m >> k >> s;
    s--;
    int x, y, z;

    for (int i = 0; i < m; i++) {
        cin >> x >> y >> z;
        x--, y--;
            
        edg.pb({ { x, y }, z });
    }


    dist.resize(n, INF);

    dist[s] = 0;

    for (int i = 0; i < k; i++) {
        vector<ll> dist_temp(n, INF);
        for (auto edge : edg) {
            if (dist[edge.x.x] != INF) {
                if (dist_temp[edge.x.y] > dist[edge.x.x] + edge.y) {
                    dist_temp[edge.x.y] = dist[edge.x.x] + edge.y;
                }
            }
        }
        dist = dist_temp;
    }

    for (auto it : dist) {
        if (it == INF)
            cout << -1 << endl;
        else
            cout << it << endl;
    }
}