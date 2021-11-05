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

#define x first
#define y second
#define sq(a) ((a) * (a))

using namespace std;

typedef long long ll;
typedef long double ld;
typedef unsigned long long ull;

const ld EPS = 0.0000001;

signed main()
{
	ios::sync_with_stdio();
	cin.tie();
	cout.tie();

	ld c;

	cin >> c;

	ld l = 0.0, r = 1000000.0, m, d;

	while (r - l > EPS)
	{
		m = (l + r) / 2;
		d = m * m + sqrt(m);
		if (d > c)
			r = m;
		else
			l = m;
	}

	cout << fixed << setprecision(6) << r << endl;
}