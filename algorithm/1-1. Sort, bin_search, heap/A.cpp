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

ll n, a[200000];

void sort(ll l, ll r)
{
	int i = l, j = r;
	int m = a[rand() % (r - l + 1) + l];
	while(i <= j)
	{
		while(a[i] < m)
			i++;
		while(a[j] > m)
			j--;
		if(i <= j)
			swap(a[i++], a[j--]);
	}
	if(i < r)
		sort(i, r);
	if(l < j)
		sort(l, j);
}

signed main()
{
    ios::sync_with_stdio();
    cin.tie();
    cout.tie();

 	cin >> n;
 	
  	for(int i = 0; i < n; i++)
  		cin >> a[i];

  	sort(0, n - 1);

  	for(int i = 0; i < n; i++)
  		cout << a[i] << ' ';
 	return 0;
}