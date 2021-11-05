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
ld vp, vf;
ld a;

ld dist(ld x, ld y, ld x1, ld y1)
{
	return (sqrt(sq(x - x1) + sq(y - y1)));
}

ld check(ld m)
{
	ld t = dist(0, 1, m, a) / vp;
	ld t1 = dist(1, 0, m, a) / vf;

	return t + t1;
}

signed main()
{
	ios::sync_with_stdio();
	cin.tie();
	cout.tie();

	cin >> vp >> vf >> a;

	ld l = 0, r = 1, m1, m2;

	while (r - l > EPS)
	{
		m1 = (l * 2 + r) / 3;
		m2 = (l + r * 2) / 3;
		if (check(m1) < check(m2))
			r = m2;
		else
			l = m1;
	}
	
	cout << fixed << setprecision(6) << (l + r) / 2;
}