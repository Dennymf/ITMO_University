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

ll w, h, n;

bool check(ll m)
{
	ll k = m / w;
	if(!k)
		return 1;
	ll k1 = n / k + !!(n % k);

	//cout << k << ' ' << k1 << ' ' << k1 * h << ' ' << m << endl;
	if(k1 * h > m)
		return 1;
	else
		return 0;
}
signed main()
{
	ios::sync_with_stdio();
	cin.tie();
	cout.tie();

	cin >> w >> h >> n;

	if(w > h)
		swap(w, h);
	ll l = 0, r = 1e18, m;

	while(r - l > 1)
	{
		m = l + (r - l) / 2;
		if(check(m))
			l = m;
		else
			r = m;
	}

	cout << r;
}