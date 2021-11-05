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

vector<int> prefix_function;

vector<int> cmp(string s)
{
    vector<int> prefix_function;
    prefix_function.resize(s.size());
    prefix_function[0] = 0;

    for (int i = 1; i < s.size(); i++)  
    {
        int j = prefix_function[i - 1];
        while (j && s[i] != s[j])
            j = prefix_function[j - 1];
        if (s[i] == s[j])
            j++;
        prefix_function[i] = j;
    }

    return prefix_function;
}

int main() {
    ios::sync_with_stdio(false);
	
	file();

    string s;
    cin >> s;

    prefix_function = cmp(s);
    for (int i = 0; i < prefix_function.size(); i++)
    {
        cout << prefix_function[i] << " ";
    }
}