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

const ll MOD = 998244353;
const ll MOD2 = MOD - 2;

ll maxdeg = 201;
ll n, m;

ll mulMod2(ll a, ll b)
{
    return (a * b) % MOD;
}

ll sumMod2(ll a, ll b)
{
    return (a + b) % MOD;
}

ll subMod2(ll a, ll b)
{
    return (a - b + MOD);
}

ll divMod2(ll a, ll b)
{
    return ((a % MOD) / (b % MOD)) % MOD;
}

ll binPow(ll a, ll n)
{
    if (n == 0) return 1;

    if (n & 1)
    {
        return mulMod2(binPow(a, n - 1), a);
    }
    else
    {
        ll b = binPow(a, n / 2);
        return mulMod2(b, b);
    }
}

ll reverseElement(ll a)
{
    return binPow(a, MOD2);
}

vector<ll> fpsSum(const vector<ll>& a, const vector<ll>& b)
{
    vector<ll> sum(maxdeg + 1, 0);

    for (ll i = 0; i <= maxdeg; ++i)
    {
        sum[i] = sumMod2(a[i], b[i]);
    }

    return sum;
}

vector<ll> compressPolynom(vector<ll>& p)
{
    size_t deg = p.size() - 1;
    while (deg > 0 && !p[deg]) deg--;

    p.resize(deg + 1);
    return p;
}

vector<ll> polyMul(const vector<ll>& a, const vector<ll>& b)
{
    vector<ll> ans(a.size() + b.size() + 3, 0);

    for (size_t i = 0; i < a.size(); ++i)
    {
        if (!a[i]) continue;

        for (size_t j = 0; j < b.size(); ++j)
        {
            if (i + j >= m + 1) break;
            ans[i + j] = sumMod2(ans[i + j], mulMod2(a[i], b[j]));
        }
    }

    return compressPolynom(ans);
}

ll getKthRatioTaylorSqrt(ll k)
{
    ll x = 1, y = 1;
    for (int i = 0; i < k; ++i)
    {
        x = mulMod2(x, subMod2(1, 2 * i));
        y = mulMod2(y, 2 * i + 2);
    }

    return mulMod2(x, reverseElement(y));
}

ll ansSqrt[100];
ll ansExp[100];
ll ansLn[100];

void taylor(vector<ll>p, const int dg)
{
    const ll minus1Mod = -1ll + MOD;

    ll ratioSqrt = 1;
    ll ratioExp = 1;
    ll ratioLn = 0;

    for (size_t i = 0; i < dg; i++)
    {
        ansSqrt[i] = ansExp[i] = ansLn[i] = 0;
    }

    vector<ll> ppow;
    ppow.push_back(1ll);

    ansSqrt[0] = 1;
    ansExp[0] = 1;
    ansLn[0] = 0;

    ll f = 1;

    for (int i = 1; i < dg; ++i)
    {
        ppow = polyMul(ppow, p);

        ratioSqrt = getKthRatioTaylorSqrt(i);

        f = mulMod2(f, i);
        ratioExp = reverseElement(f);
        ratioLn = mulMod2((i & 1 ? 1ll : minus1Mod), reverseElement(i));

        for (int j = 0; j < dg; ++j)
        {
            ll z = (j < ppow.size() ? ppow[j] : 0);

            ansSqrt[j] = sumMod2(ansSqrt[j], mulMod2(z, ratioSqrt));
            ansExp[j] = sumMod2(ansExp[j], mulMod2(z, ratioExp));
            ansLn[j] = sumMod2(ansLn[j], mulMod2(z, ratioLn));
        }
    }

    for (size_t i = 0; i < dg; i++)
    {
        cout << ansSqrt[i] << " ";
    }
    cout << '\n';

    for (size_t i = 0; i < dg; i++)
    {
        cout << ansExp[i] << " ";
    }
    cout << '\n';

    for (size_t i = 0; i < dg; i++)
    {
        cout << ansLn[i] << " ";
    }
    cout << '\n';
}


int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    cin >> n >> m;
    vector<ll> p(n + 1, 0ll);

    for (int i = 0; i <= n; ++i)
    {
        cin >> p[i];
    }

    taylor(p, static_cast<const int>(m));
}