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

int deg = 3000;

vector<ll> polyMul(vector<ll> a, vector<ll> b, int maxDeg)
{
    vector<ll> ans(maxDeg, 0);

    for (size_t i = 0; i < a.size(); i++)
    {
        for (size_t j = 0; j < b.size(); j++)
        {
            if (i + j >= maxDeg)
                continue;

            ans[i + j] += a[i] * b[j];
        }
    }

    return ans;
}

int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    int k;

    cin >> k;

    vector<ll> a(k), c(k);

    for (int i = 0; i < k; i++)
        cin >> a[i];
    for (int i = 0; i < k; i++)
        cin >> c[i];

    vector<ll> q;
    q.push_back(1ll);

    for (const auto it : c)
    {
        q.push_back(-it);
    }

    vector<ll> p = a;
    p = polyMul(p, q, k);

    ll degp = p.size() - 1;
    for (auto rit = p.rbegin(); rit != p.rend(); rit++)
    {
        if (*rit != 0) break;
        degp--;
    }
    cout << degp << endl;
    for (int i = 0; i <= degp; i++)
        cout << p[i] << " ";
    
    cout << '\n' << q.size() - 1 << '\n';

    for (auto it : q)
        cout << it << " ";
}