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

ll gcd(ll a, ll b)
{
    if (b == 0) return a;
    return gcd(b, a % b);
}

vector<ll> polySum(vector<ll> a, vector<ll> b)
{
    vector<ll> sum(max(a.size(), b.size()), 0);

    for (ll i = 0; i < sum.size(); ++i)
    {
        ll x = (i < a.size() ? a[i] : 0);
        ll y = (i < b.size() ? b[i] : 0);

        sum[i] = x + y;
    }

    return sum;
}

vector<ll> compressPolynom(vector<ll> p)
{
    size_t deg = p.size() - 1;
    while (deg > 0 && !p[deg]) deg--;

    p.resize(deg + 1);
    return p;
}

vector<ll> polyMul(vector<ll> a, vector<ll> b)
{
    vector<ll> ans(a.size() + b.size() + 5, 0);

    for (int i = 0; i < ans.size(); ++i)
    {
        for (int j = 0; j < i + 1; ++j)
        {
            ll x = j >= a.size() ? 0 : a[j];
            ll y = (i - j) >= b.size() ? 0 : b[i - j];
            ans[i] += x * y;
        }
    }


    return compressPolynom(ans);
}

ll fact(ll x)
{
    ll res = 1;

    for (ll i = 2; i <= x; ++i)
    {
        res *= i;
    }

    return res;

}

ll binPow(ll a, ll n)
{
    if (n == 0) return 1;

    if (n & 1)
    {
        return binPow(a, n - 1) * a;
    }
    else
    {
        ll b = binPow(a, n / 2);
        return b * b;
    }
}

int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    ll r, k;
    cin >> r >> k;

    vector<ll> p(static_cast<unsigned int>(k + 1));
    ll m = -1;

    for (size_t i = 0; i <= k; i++)
    {
        cin >> p[i];
    }

    for (int i = p.size() - 1; i >= 0; i--)
    {
        if (p[i] != 0)
        {
            m = i;
            break;
        }
    }

    vector<ll> sum{ 0 };

    for (ll i = 0; i <= m; i++)
    {
        vector<ll> q{ 1 };

        for (ll j = 1; j <= k; j++)
        {
            vector<ll> qq{ j - i, 1 };
            q = polyMul(q, qq);
        }

        ll rm_i = binPow(r, m - i);
        ll coeff = rm_i * p[i];

        for (long long& elem : q)
        {
            elem *= coeff;
        }

        sum = polySum(sum, q);
    }


    ll denom = fact(k) * binPow(r, m);

    vector<ll> nums(sum.size()), denoms(sum.size());

    for (size_t i = 0; i < sum.size(); i++)
    {
        ll nod;
        if (sum[i] > 0)
            nod = gcd(sum[i], denom);
        else
            nod = gcd(-sum[i], denom);
        nums[i] = sum[i] / nod;
        denoms[i] = denom / nod;
    }


    for (size_t i = 0; i < sum.size(); i++)
    {
        cout << nums[i] << "/" << denoms[i] << " ";
    }
    cout << endl;
}