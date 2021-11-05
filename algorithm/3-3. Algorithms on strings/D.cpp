#include <iostream>
#include <cmath>
#include <algorithm>
#include <iomanip>
#include <vector>
#include <set>
#include <map>
#include <deque>
#include <queue>
#include <stack>
#include <map>
#include <random>
#include <string>
#include <cstring>
#include <unordered_set>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 5 * 1e6 + 100;
const ll M = 1e4 + 100;
const int INF = 1e9 + 123;
const ll MOD = 1e9 + 7;

using namespace std;

#define x first
#define y second
#define pb push_back

void file()
{
	//freopen("avia.in", "r", stdin);
	//freopen("avia.out", "w", stdout);
}

vector<int> z_function;

vector<int> zFunction(string s)
{
    vector<int> z_function(s.size());
    for (int i = 1, left = 0, right = 0; i < s.size(); i++)
    {
        if (i <= right)
            z_function[i] = min(right - i + 1, z_function[i - left]);
        while (i + z_function[i] < s.size() && s[i + z_function[i]] == s[z_function[i]])
            z_function[i]++;
        if (i + z_function[i] - 1 > right)
        {
            left = i;
            right = i + z_function[i] - 1;
        }

    }

    return z_function;
}

int main() {
    ios::sync_with_stdio(false);
	
	file();

    string s, t;
    cin >> s >> t;

    int start = s.size() + 1;
    s += "#" + t;
    z_function = zFunction(s);

    vector<int> ans;

    for (int i = start; i < z_function.size(); i++)
        if (z_function[i] == start - 1)
            ans.pb(i - start + 1);
    
    cout << ans.size() << "\n";

    for (int i = 0; i < ans.size(); i++)
        cout << ans[i] << " ";

    return 0; 
}