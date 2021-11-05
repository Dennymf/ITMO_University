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

#define sq(a) ((a) * (a))

typedef long long ll;
typedef long double ld;

using namespace std;

ll bin_pow(ll a, ll n, ll mod)
{
    if (n == 0) return 1;

    if (n % 2 == 0)
    {
        ll b = bin_pow(a, n / 2, mod);
        return (b * b) % mod;
    }
    else
    {
        return (a * bin_pow(a, n - 1, mod)) % mod;
    }
}

ll gcd_ex(ll a, ll b, ll& x, ll& y)
{
    if (!a)
    {
        x = 0;
        y = 1;
        return b;
    }

    ll x1, y1;
    ll ans = gcd_ex(b % a, a, x1, y1);
    x = y1 - (b / a) * x1;
    y = x1;

    return ans;
}

ll rev(ll a, ll mod)
{
    ll x, y;
    ll gcd = gcd_ex(a, mod, x, y);

    x = (x % mod + mod) % mod;
    return x;
}

int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);
    srand(time(0));

    ll n, e, c;

    cin >> n >> e >> c;

    ll t;

    for (ll i = 2; i * i <= n; i++)
    {
        if (n % i == 0)
        {
            t = (i - 1) * (n / i - 1);
            break;
        }
    }

    cout << bin_pow(c, rev(e, t), n);
}