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

int n, m;
vector<ll> a, b;
int deg = 3000;

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
    ll x = ((a % MOD) - (b % MOD)) % MOD;
    return x > 0 ? x : x + MOD;
}

ll divMod2(ll a, ll b)
{
    return ((a % MOD) / (b % MOD)) % MOD;
}

vector<ll> fpsSum(vector<ll> a, const vector<ll> b)
{
    vector<ll> sum(deg + 1, 0);

    for (int i = 0; i <= deg; i++)
    {
        sum[i] = sumMod2(a[i], b[i]);
    }

    return sum;
}

ll getKthCoeffMul(ll k, vector<ll> a, vector<ll> b)
{
    ll coeff = 0;

    for (int i = 0; i <= k; i++)
    {
        coeff = sumMod2(coeff, mulMod2(a[i], b[k - i]));
    }

    return coeff;
}

vector<ll> fpsMul(vector<ll> a, vector<ll> b)
{
    vector<ll> mul(deg, 0);

    for (int i = 0; i < mul.size(); i++)
    {
        mul[i] = getKthCoeffMul(i, a, b);
    }

    return mul;
}

ll getKthCoeffDiv(ll k, vector<ll> a, vector<ll> b, vector<ll> c)
{
    ll sum = 0;
    for (size_t i = 1; i <= k; i++)
    {
        sum = sumMod2(sum, mulMod2(b[i], c[k - i]));
    }

    return divMod2(subMod2(a[k], sum), b[0]);

}

vector<ll> fpsDiv(vector<ll> a, vector<ll> b)
{
    vector<ll> div(deg + 1, 0);

    for (size_t k = 0; k <= deg; k++)
    {
        div[k] = getKthCoeffDiv(k, a, b, div);
    }

    return div;
}

int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    cin >> n >> m;

    a.resize(deg + 1);
    b.resize(deg + 1);

    for (int i = 0; i <= n; i++)
        cin >> a[i];
    for (int i = 0; i <= m; i++)
        cin >> b[i];

    for (int i = n + 1; i <= deg; i++)
        a[i] = 0;
    for (int i = m + 1; i <= deg; i++)
        b[i] = 0;

    vector<ll> sum = fpsSum(a, b);

    int t = max(n, m);
    cout << t << '\n';
    for (int i = 0; i <= t; i++)
        cout << sum[i] << " ";

    vector<ll> mul = fpsMul(a, b);
    t = n + m;

    cout << '\n' << t << '\n';
    for (int i = 0; i <= t; i++)
        cout << mul[i] << " ";

    cout << '\n'
    vector<ll> div = fpsDiv(a, b);
    for (int i = 0; i < 1000; i++)
        cout << div[i] << " ";
}