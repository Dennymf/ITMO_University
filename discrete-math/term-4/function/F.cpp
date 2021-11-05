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

const ll MOD = 1e9 + 7;

ll mulMod2(ll a, ll b)
{
    return ((a % MOD) * (b % MOD)) % MOD;
}

ll sumMod2(ll a, ll b)
{
    return ((a % MOD) + (b % MOD)) % MOD;
}

int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    int k, m;
    cin >> k >> m;

    vector<ll> weight(k, 0);
    vector<ll> dp(m + 1, 0);
    vector<ll> cntTrees(m + 1, 0);
    unordered_set<ll> weights;

    for (int i = 0; i < k; i++)
    {
        cin >> weight[i];
        weights.insert(weight[i]);
    }

    dp[0] = cntTrees[0] = 1;

    for (ll treew = 1; treew <= m; treew++)
    {
        for (auto w : weight)
        {
            if (w > treew) continue;
            cntTrees[treew] = sumMod2(cntTrees[treew], dp[treew - w]);
        }

        for (ll rootw = 0; rootw <= treew; rootw++)
        {
            ll subtreesw = treew - rootw;

            dp[treew] = sumMod2(dp[treew], (mulMod2(cntTrees[rootw], cntTrees[subtreesw])));
        }
    }

    for (int i = 1; i < cntTrees.size(); i++)
    {
        cout << cntTrees[i] << " ";
    }
}