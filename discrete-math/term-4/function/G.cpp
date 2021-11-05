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

using namespace std;

const ll MOD = 104857601;

vector<ll> getLast(vector<vector<ll>>& v)
{
    vector<ll> b = v.back();
    v.pop_back();
    return b;
}

vector<ll> emptyObj()
{
    return vector<ll>(7, 0ll);
}

vector<ll> buildPair(vector<ll> f, vector<ll> s)
{
    vector<ll> res = emptyObj();
    for (int i = 0; i < f.size(); i++){
        if (f[i] == 0)
            continue;
        for (int j = 0; j < s.size(); j++){
            if (i + j >= 7) break;
            res[i + j] += f[i] * s[j];
        }
    }

    return res;
}

ll fact(ll x)
{
    ll res = 1;

    for (ll i = 2; i <= x; ++i){
        res *= i;
    }

    return res;
}

ll combinations(ll n, ll k)
{
    if (k == 0 || k == n)
        return 1;

    ll num = 1;

    for (ll i = 1; i <= k; i++){
        num *= (n - i + 1);
    }

    return num / fact(k);
}

vector<ll> buildMSet(vector<ll> obj)
{
    vector<vector<ll>> dp(7);
    for (int i = 0; i < dp.size(); i++){
        if (!i){
            dp[i].resize(7, 1);
        }
        else {
            dp[i].resize(7, 0);
        }
    }

    for (int i = 1; i < 7; i++){
        for (int j = 1; j < 7; j++){
            if (!obj[j]){
                dp[i][j] = dp[i][j - 1];
                continue;
            }
            for (size_t ind = 0; ind <= i / j; ind++){
                ll n_ = obj[j] - 1 + ind;
                dp[i][j] += dp[i - j * ind][j - 1] * combinations(n_, ind);
            }
        }
    }

    vector<ll> res;
    for (int i = 0; i < 7; i++){
        res.push_back(dp[i][i]);
    }

    return res;
}

vector<ll> buildSeq(vector<ll> obj){
    vector<ll> res = emptyObj();

    for (int i = 0; i < 7; i++){
        if (!i){
            res[i] = 1;
            continue;
        }

        for (int j = 1; j <= i; j++){
            res[i] += res[i - j] * obj[j];
        }
    }

    return res;
}

int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    string s;
    getline(cin, s);

    reverse(s.begin(), s.end());
    vector<vector<ll>> objects;

    for (size_t i = 0; i < s.size(); i++)
    {
        if (s[i] == 'S' || s[i] == 'L')
        {
            vector<ll> obj = getLast(objects);
            objects.push_back(s[i] == 'S' ? buildMSet(obj) : buildSeq(obj));
        }
        else if (s[i] == 'P')
        {
            vector<ll> obj1 = getLast(objects);
            vector<ll> obj2 = getLast(objects);
            objects.push_back(buildPair(obj1, obj2));
        }
        else if (s[i] == 'B')
        {
            vector<ll> B = emptyObj();
            B[1] = 1;
            objects.push_back(B);
        }
    }

    vector<ll> ans = getLast(objects);

    for (int i = 0; i < ans.size(); i++)
        cout << ans[i] << " ";
}