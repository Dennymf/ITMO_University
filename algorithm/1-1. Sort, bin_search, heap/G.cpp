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

ll n, x, y;

signed main()
{
    ios::sync_with_stdio();
    cin.tie();
    cout.tie();

 	ll n, x, y;

 	cin >> n >> x >> y;

 	ll t = min(x, y);
 	n--;

 	ll m, l = -1, r = 1e10;

 	while(r - l > 1)
 	{
 		m = l + (r - l) / 2;
 		if(m / x + m / y < n)
 			l = m;
 		else
 			r = m;
 	}

 	cout << r + t;
 	return 0;
}