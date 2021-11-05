#include <iostream>
#include <cmath>
#include <algorithm>
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <fstream>
#include <iomanip>
#include <map>
#include <unordered_map>
#include <set>
#include <unordered_set>
#include <vector>
#include <queue>
#include <deque>
#include <stack>
#include <ctime>
#include <string>
#include <vector>

#define x first
#define y second
#define sq(a) ((a) * (a))

using namespace std;

typedef long long ll;
typedef long double ld;
typedef unsigned long long ull;

const ll INF = 1e10;

ll dp[20][10000], a[20][10000];
int c[20][20], n;
vector<int> p;

int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	//	freopen("input.txt", "r", stdin);
	//	freopen("output.txt", "w", stdout);

	cin >> n;

	ll k = 1 << n;
	int x = k - 1;

	for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++)
			cin >> c[i][j];

	for (int i = 0; i < n; i++)
		for (int j = 0; j < k; j++)
			if (!j)
				dp[i][j] = 0;
			else
				dp[i][j] = INF;

	for (int i = 0; i < k; i++) 
		for (int j = 0; j < n; j++) 
			if ((i >> j & 1) == 1)
				for (int r = 0; r < n; r++) {
					ll t = 1 << j;
					if (dp[r][i - t] + c[j][r] < dp[j][i])
						dp[j][i] = dp[r][i - t] + c[j][r], a[j][i] = r;
				}

	int l = 0;
	ll minn = INF;

	for (int i = 0; i < n; i++)
		if (dp[i][k - 1] < minn)
			minn = dp[i][k - 1], l = i;

	for (int i = 0; i < n; i++) {
		p.push_back(l + 1);
		int t = l;
		l = a[l][x];
		ll h = 1 << t;
		x -= h;
	}

	cout << minn << endl;

	reverse(p.begin(), p.end());
	for (int i = 0; i < p.size(); i++)
		cout << p[i] << ' ';

	return 0;
}