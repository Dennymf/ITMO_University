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
#include <cstdlib>
#include <cstdio>

#define x first
#define y second
#define sq(a) ((a) * (a))

typedef long long ll;
typedef long double ld;

using namespace std;

int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);
    srand(time(0));

    ll a, b, n, m;

    cin >> a >> b >> n >> m;

    if (n < m)
    {
        swap(a, b);
        swap(n, m);
    }

    for (ll i = 0; i < n; i++)
    {
        ll s = i * n + a;
        if (s % m == b)
        {
            cout << s;
            return 0;
        }
    }
}