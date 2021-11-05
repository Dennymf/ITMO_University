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

int w[200][200];
int dist[200][200];

int main() {
    file();

    int n;

    cin >> n;

    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
        {
            cin >> w[i][j];
            dist[i][j] = w[i][j];
        }

    for (int i = 0; i < n; i++)
        for (int u = 0; u < n; u++)
            for (int v = 0; v < n; v++)
                dist[u][v] = min(dist[u][v], dist[u][i] + dist[i][v]);

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++)
            cout << dist[i][j] << " ";
        cout << endl;
    }
}