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

const ll INF = 1e11;

void file()
{
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);     
}   

ll n, k, a[200000], b[200000];

int bin_search(ll y)
{
	ll l = 0, r = n - 1;
	while(r - l > 1)
	{
		ll m = l + (r - l) / 2;
		if(a[m] < y)
			l = m;
		else
			r = m;
	}
	if(y - a[l] <= a[r] - y)
		return a[l];
	else
		return a[r];
}

signed main()
{
    ios::sync_with_stdio();
    cin.tie();
    cout.tie();

 	cin >> n >> k;

  	for(int i = 0; i < n; i++)
  		cin >> a[i];

  	for(int i = 0; i < k; i++)
  		cin >> b[i];

  	for(int i = 0; i < k; i++)
  		cout << bin_search(b[i]) << endl;
 	return 0;
}