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
#include <chrono>
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

void file()
{
//	freopen("problem4.in", "r", stdin);
//	freopen("problem4.out", "w", stdout);
}

int dp[200000][100], p[200000];

int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	int n;

	cin >> n;

	for (int i = 1; i <= n; i++)
	{
		int x;
		cin >> x;

		p[i] = x;
	}

	for (int i = 1; i <= n; i++)
	{
		dp[i][0] = p[i];
	}

	int l = 0, n1 = 1;
	while (n1 < n)
	{
		n1 *= 2, l++;
	}

	for (int j = 1; j <= l; j++)
		for (int i = 1; i <= n; i++)
			dp[i][j] = dp[dp[i][j - 1]][j - 1];

	for (int i = 1; i <= n; i++)
	{
		cout << i << ":";
		for (int j = 0; j <= l; j++)
		{
			if (!dp[i][j])
				break;
			cout << " " << dp[i][j];
		}
		cout << endl;
	}
	return 0;
}