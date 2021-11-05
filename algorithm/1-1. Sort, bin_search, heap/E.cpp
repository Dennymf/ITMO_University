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

void file()
{
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);     
}   

ll n;
ll a[200000];

pair<ll, ll> bin_search(ll ale, ll b)
{
	ll l = -1, r = n;
	ll m;

	pair<ll, ll> ans;

	while(r - l > 1)
	{
		m = l + (r - l) / 2;
		if(a[m] < ale)
			l = m;
		else
			r = m;
	}

	ans.x = r;

	l = -1, r = n + 1;

	while(r - l > 1)
	{
		m = l + (r - l) / 2;
		if(a[m] <= b)
			l = m;
		else
			r = m;
	}

	ans.y = l;

	return ans;
}
signed main()
{
	ios::sync_with_stdio();
	cin.tie();
	cout.tie();

	cin >> n;

	a[n] = 1e17;
	for(int i = 0; i < n; i++)
		cin >> a[i];

	sort(a, a + n);

	ll k, l, r;
	pair<ll, ll> ind;

	cin >> k;

	for(int i = 0; i < k; i++)
	{
		cin >> l >> r;
		ind = bin_search(l, r);
		//cout << ind.x << ' ' << ind.y << endl;
		cout << ind.y - ind.x + 1<< endl;
	}
}