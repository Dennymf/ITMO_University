#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <algorithm>

using namespace std;

#define x first
#define y second
#define sq(a) ((a) * (a))

typedef long long ll;
typedef long double ld;
typedef unsigned long ul;

const ll N = 4e6 + 10;
const ll INF = 1e18;

int n;
int a[N];
ll pref[N];
ll t[150][150][150];

int funct(int i) {
	return i & (i + 1);
}


ll sum(int x, int y, int z) {
	ll s = 0;

	for (int i = x; i >= 0; i = funct(i) - 1) {
		for (int j = y; j >= 0; j = funct(j) - 1) {
			for (int k = z; k >= 0; k = funct(k) - 1) {
				s += t[i][j][k];
			}
		}
	}

	return s;
}

void update(int x, int y, int z, int d) {
	for (int i = x; i < n; i = i | (i + 1)) {
		for (int j = y; j < n; j = j | (j + 1)) {
			for (int k = z; k < n; k = k | (k + 1)) {
				t[i][j][k] += d;
			}
		}
	}
}

int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	cin >> n;

	int m = 500;
	while (m != 3) {
		cin >> m;

		if (m == 2) {
			int x1, y1, z1, x2, y2, z2;
			cin >> x1 >> y1 >> z1 >> x2 >> y2 >> z2;
			cout << sum(x2, y2, z2) - sum(x2, y1 - 1, z2) - sum(x1 - 1, y2, z2)
				- sum(x2, y2, z1 - 1) + sum(x2, y1 - 1, z1 - 1) + sum(x1 - 1, y2, z1 - 1)
				+ sum(x1 - 1, y1 - 1, z2) - sum(x1 - 1, y1 - 1, z1 - 1) << endl;
			continue;
		}

		if (m == 1) {
			int x, y, z, d;
			cin >> x >> y >> z >> d;
			update(x, y, z, d);
			continue;
		}


	}

	return 0;
}