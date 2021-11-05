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
#define ll __int128

typedef long double ld;

using namespace std;

pair<ll, ll> to2Degs(ll n)
{
    ll cnt = 0;
    while (n % 2 == 0)
    {
        cnt++;
        n /= 2;
    }

    return { cnt, n };
}

ll bin_pow(ll a, ll n, ll mod)
{
    if (n == 0)
        return 1;

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

bool miller_rabin(ll n, int times)
{
    if (n == 1)
        return false;
    if (n == 2 || n == 3)
        return true;

    pair<ll, ll> p = to2Degs(n - 1);
    ll s = p.x;
    ll t = p.y;

    for (int cnt = 0; cnt < times; cnt++)
    {
        ll a = rand() % (n - 1) + 1;
        while (a == 1 || a == n - 1)
        {
            a = rand() % (n - 1) + 1;
        }
        ll x = bin_pow(a, t, n);

        if (x == 1 || x == n - 1)
        {
            continue;
        }

        bool cont = false;
        for (long long i = 0; i < s - 1; i++)
        {
            x = (x * x) % n;

            if (x == 1)
            {
                return false;
            }

            if (x == n - 1)
            {
                cont = true;
                break;
            }
        }
        if (!cont)
        {
            return false;
        }
    }

    return true;
}

int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);
    srand(time(0));

    int n;

    cin >> n;

    for (int i = 0; i < n; i++)
    {
        long long a;
        cin >> a;

        if (miller_rabin(a, 10))
            cout << "YES\n";
        else
            cout << "NO\n";
    }

}