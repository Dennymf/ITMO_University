#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <vector>
#include <map>
#include <unordered_set>

#define sq(a) ((a) * (a))

typedef long long ll;

using namespace std;

const ll MOD = 104857601;
ll k;
int maxdeg;

ll mulMod2(ll a, ll b)
{
    return ((a % MOD) * (b % MOD)) % MOD;
}

ll sumMod2(ll a, ll b)
{
    return ((a % MOD) + (b % MOD)) % MOD;
}

ll subMod2(ll a, ll b)
{
    ll x = (a - b) % MOD;
    return x >= 0 ? x : x + MOD;
}

vector<ll> polyMul(vector<ll> a, vector<ll> b, int maxDeg)
{
    vector<ll> ans(a.size() + b.size() + 5, 0);

    for (int i = 0; i < ans.size(); i = i + 2)
    {
        for (int j = 0; j < i + 1; j++)
        {
            ll x = j >= a.size() ? 0 : a[j];
            ll y = (i - j) >= b.size() ? 0 : b[i - j];
            ans[i] = sumMod2(ans[i], mulMod2(x, y));
        }
    }

    return ans;
}

ll calcNth(ll n, vector<ll> a, vector<ll> q)
{
    while (n >= k)
    {
        for (int i = k; i <= 2 * k - 1; i++)
        {
            a[i] = 0;
            for (ll j = 1; j <= k; j++)
            {
                a[i] = subMod2(a[i], mulMod2(q[j], a[i - j]));
            }
        }

        vector<ll> _q(q.size());
        for (int i = 0; i < k + 1; i++)
        {
            if (i % 2 == 0)
            {
                _q[i] = q[i];
            }
            else
            {
                _q[i] = subMod2(MOD, q[i]);
            }
        }

        vector<ll> r = polyMul(q, _q, maxdeg);
        for (int i = 0; i <= k; i++)
        {
            q[i] = r[i * 2];
        }

        int ind = n % 2;
        for (int i = ind; i <= 2 * k - 1; i += 2)
        {
            a[i / 2] = a[i];
        }

        n /= 2;
    }

    return a[n];
}

int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    ll n;
    cin >> k >> n;
    maxdeg = 2 * k + 1;
    n--;

    vector<ll> a(maxdeg);

    for (int j = 0; j < k; j++)
    {
        cin >> a[j];
    }

    vector<ll> q(k + 2);
    q[0] = 1ll;

    for (int i = 1; i <= k; i++)
    {
        ll z;
        cin >> z;
        q[i] = subMod2(MOD, z);
    }

    cout << calcNth(n, a, q);
}