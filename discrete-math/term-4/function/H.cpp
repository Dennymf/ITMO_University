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
const ll MOD2 = MOD - 2;

int maxdeg = 11000;
int n;

ll sumMod2(ll a, ll b){
    return (a + b) % MOD;
}

ll subMod2(ll a, ll b){
    return (a - b + MOD) % MOD;
}

ll mulMod2(ll a, ll b){
    return (a * b) % MOD;
}

ll binPow(ll a, ll n){
    if (n == 0) return 1;

    if (n & 1) {
        return mulMod2(binPow(a, n - 1), a);
    }
    else{
        ll b = binPow(a, n / 2);
        return mulMod2(b, b);
    }
}

ll reverseElement(ll a){
    return binPow(a, MOD2);
}

inline ll fact(ll x){
    ll res = 1;

    for (ll i = 2; i <= x; ++i) {
        res = mulMod2(res, i);
    }

    return res;
}

ll combinations(ll n, ll k) {
    if (k == 0 || k == n)
        return 1;

    ll num = 1;

    for (ll i = 1; i <= k; i++) {
        ll tmp = n - i + 1;
        if (tmp < 0)
            tmp += MOD;

        num = mulMod2(num, tmp);
    }

    return mulMod2(num, reverseElement(fact(k)));
}

ll getRatioDiv(ll k, vector<ll> &a, vector<ll> &b, vector<ll> &c) {
    ll sum = 0;
    for (ll i = 1; i <= k; ++i){
        sum = sumMod2(sum, mulMod2(b[i], c[k - i]));
    }

    return subMod2(a[k], sum);
}

vector<ll> fpsDiv(vector<ll> &a, vector<ll> &b) {
    vector<ll> div(n + 1, 0);

    for (ll k = 0; k <= n; k++) {
        div[k] = getRatioDiv(k, a, b, div);
    }

    return div;
}

int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    ll k;
    cin >> k >> n;

    vector<ll> p(n + 1, 0), q(n + 1, 0);
    vector<ll> ones = { 1, -1 + MOD };

    for (int i = 0; i <= (k - 2) / 2; i++)
    {
        ll k1 = ones[i % 2];
        ll tmp = subMod2(k, i + 2);
        ll c1 = combinations(tmp, i);
        p[i + 1] = mulMod2(k1, c1);
    }

    for (int i = 0; i <= (k - 1) / 2; i++)
    {
        ll k1 = ones[i % 2];
        ll tmp = subMod2(k, i + 1);
        if (tmp < 0) tmp += MOD;
        ll c2 = combinations(tmp, i);
        q[i] = mulMod2(k1, c2);
    }

    vector<ll> d = fpsDiv(p, q);

    for (int i = 1; i <= n; i++)
    {
        cout << d[i] << endl;
    }
}