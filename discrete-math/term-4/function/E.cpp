#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <vector>
#include <map>
#include <unordered_set>
#include <string>

#define sq(a) ((a) * (a))

typedef long long ll;
typedef long double ld;

using namespace std;

const ll MOD = 998244353;

inline ll binPow(ll a, ll n)
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

ll fact(ll x)
{
    ll res = 1;

    for (ll i = 2; i <= x; ++i){
        res *= i;
    }

    return res;
}

vector<ld> compressPolynom(vector<ld>& p)
{
    const ld eps = 1e5;
    int deg = p.size() - 1;
    while (deg > 0 && p[deg] == 0) deg--;

    p.resize(deg + 1);
    return p;
}

vector<ld> polyMul(vector<ld> a, vector<ld> b)
{
    vector<ld> ans(a.size() + b.size() + 5, 0);

    for (int i = 0; i < ans.size(); ++i)
    {
        for (int j = 0; j < i + 1; ++j)
        {
            ld x = j >= a.size() ? 0 : a[j];
            ld y = (i - j) >= b.size() ? 0 : b[i - j];
            ans[i] += x * y;
        }
    }


    return compressPolynom(ans);
}

vector<ld> gauss(vector<vector<ld>> a, vector<ld> b)
{
    ll k = 0, ind;
    const ld eps = 1e-5;
    vector<ld> res(a.size());
    ld mx;

    while (k < a.size()) {
        mx = abs(a[k][k]);
        ind = k;

        for (ll i = k + 1; i < a.size(); i++) {
            if (mx < abs(a[i][k])) {
                mx = abs(a[i][k]);
                ind = i;
            }
        }

        for (ll j = 0; j < a.size(); j++) {
            swap(a[k][j], a[ind][j]);
        }

        swap(b[k], b[ind]);

        for (ll i = k; i < a.size(); i++) {
            ld z = a[i][k];
            if (eps > abs(z))
                continue;

            for (ll j = 0; j < a.size(); j++) {
                a[i][j] /= z;
            }

            b[i] /= z;

            if (i == k)
                continue;

            for (ll j = 0; j < a.size(); j++) {
                a[i][j] -= a[k][j];
            }

            b[i] -= b[k];
        }
        k++;
    }

    for (k = a.size() - 1; k >= 0; k--) {
        res[k] = b[k];
        for (ll i = 0; i < k; i++) {
            b[i] -= a[i][k] * res[k];
        }
    }

    return res;
}

vector<vector<ld>> transpose(const vector<vector<ld>>& v)
{
    size_t n_ = v.size();
    size_t m_ = (v.empty() ? 0 : v[0].size());
    vector<vector<ld>> res(m_, vector<ld>(n_));

    for (size_t i = 0; i < n_; i++)
    {
        for (size_t j = 0; j < m_; j++)
        {
            res[j][i] = v[i][j];
        }
    }

    return res;
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

    vector<ld> sum{ 0 };
    vector<vector<ld>> slau;
    vector<ld> fCoeffs;

    ll f = fact(k);
    ld denom = fact(k) * binPow(r, m);

    for (ll i = 0; i <= m; i++)
    {
        vector<ld> q;
        q.push_back(1);

        for (ll j = 1; j <= k; j++)
        {
            vector<ld> qq;
            qq.push_back(j - i);
            qq.push_back(1);
            q = polyMul(q, qq);
        }

        slau.push_back(q);
        fCoeffs.push_back(denom * p[i]);
    }


    for (auto& it : slau)
    {
        reverse(it.begin(), it.end());
    }

    slau = transpose(slau);

    reverse(fCoeffs.begin(), fCoeffs.end());

    vector<ld> res = gauss(slau, fCoeffs);


    res = compressPolynom(res);

    vector<ll> P(res.size());
    for (size_t i = 0; i < res.size(); i++)
    {
        P[i] = static_cast<ll>(round(res[i] / binPow(r, m - i)));
    }

    cout << P.size() - 1 << endl;
    for (int i = 0; i < P.size(); i++)
        cout << P[i] << " ";
    
    cout << endl;;


    vector<ld> q;
    q.push_back(1);
    q.push_back(-r);

    vector<ld> rs;
    rs.push_back(1);

    for (size_t i = 0; i < k + 1; i++)
    {
        rs = polyMul(rs, q);
    }

    vector<ll> Q;
    for (const auto it : rs)
    {
        Q.push_back(static_cast<ll>(it));
    }

    cout << Q.size() - 1 << endl;
    for (int i = 0; i < Q.size(); i++)
        cout << Q[i] << " ";

}