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

ll n, m;
ll a[3000], d[3000], p[3000];
vector<ll> b;

signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

//	freopen("input.txt", "r", stdin);
//	freopen("output.txt", "w", stdout);

	cin >> n;
	for (int i = 0; i < n; i++)
		cin >> a[i], p[i] = -1;

	d[0] = 1;
	for (int i = 1; i < n; i++)
	{
		d[i] = 1;
		for(int j = 0; j < i; j++)
			if (a[j] < a[i] && d[j] + 1 > d[i])
			{
				d[i] = d[j] + 1;
				p[i] = j;
			}
	}

	ll mx = 0, i1 = 0;

	for (int i = 0; i < n; i++)
		if (d[i] > mx)
			mx = d[i], i1 = i;

	while (i1 != -1)
		b.push_back(a[i1]), i1 = p[i1];

	cout << b.size() << endl;
	reverse(b.begin(), b.end());
	for (int i = 0; i < b.size(); i++)
		cout << b[i] << ' ';
}