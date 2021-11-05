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

ll ans = 0;
ll a[600000];
ll b[600000];
ll n;
void merge(ll i, ll j, ll k)
{
	ll t = 0;
	ll l = i + k, r = min(j + k, n);
	ll i1 = i;

	ll i2 = i, j2 = j;
	
	while (i2 < l || j2 < r)
	{
		if (a[i2] > a[j2] && j2 < r)
			j2++;
		else if ((a[i2] <= a[j2] || j2 == r) && i2 < l)
			ans += j2 - j, i2++;
		else
			break;
	}

	for (; i < l || j < r;)
	{
		if ((a[i] <= a[j] && i < l) || j >= r)
			b[t++] = a[i], i++;
		else if (((a[i] > a[j] && j < r) || i >= l))
			b[t++] = a[j], j++;
	}

	for (int g = 0; g < t; g++)
		a[i1 + g] = b[g];

	return;
}
signed main()
{
	ios::sync_with_stdio();
	cin.tie();
	cout.tie();

	cin >> n;

	for (int i = 0; i < n; i++)
		cin >> a[i];

	ll k = 1, i, j;

	for (; k < n;)
	{
		i = 0;
		j = i + k;
		for (; i < n;)
		{
			merge(i, j, k);
			i += k * 2;
			j = i + k;
		}
		k *= 2;
	}

	cout << ans;
	return 0;
}