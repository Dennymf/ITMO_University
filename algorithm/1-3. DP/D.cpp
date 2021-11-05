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
const ll MOD = 1e9;

int n;
ll a[300], d[300];

signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

//	freopen("input.txt", "r", stdin);
//	freopen("output.txt", "w", stdout);

	cin >> n;

	a[0] = 1;
	a[1] = 1;
	a[2] = 1;
	a[3] = 1;
	a[4] = 1;
	a[5] = 0;
	a[6] = 1;
	a[7] = 1;
	a[8] = 1;
	a[9] = 1;

	for (int j = 0; j < 10; j++)
		d[j] = a[j];

	for (int i = 0; i < n - 1; i++)
	{
		for(int j = 0; j < 10; j++)
			switch (j)
				{
				case 0: {
					d[j] = a[4] + a[6];
					break;
				}
				case 1: {
					d[j] = a[8] + a[6];
					break;
				}
				case 2:{
					d[j] = a[7] + a[9];
					break;
				}
				case 3: {
					d[j] = a[4] + a[8];
					break;
				}
				case 4: {
					d[j] = a[3] + a[9] + a[0];
					break;
				}
				case 6: {
					d[j] = a[1] + a[7] + a[0];
					break;
				}
				case 7: {
					d[j] = a[2] + a[6];
					break;
				}
				case 8: {
					d[j] = a[1] + a[3];
					break;
				}
				case 9: {
					d[j] = a[2] + a[4];
					break;
				}
				}

		for (int j = 0; j < 10; j++)
			a[j] = d[j] % MOD;
	}
	
	ll ans = 0;
	if (n == 1)
		ans++;
	for (int j = 0; j < 10; j++)
		if(j != 0 && j != 8)
			ans = (ans + a[j]) % MOD;

	cout << ans;
}