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

int a[110];

signed main()
{
    ios::sync_with_stdio();
    cin.tie();
    cout.tie();
 
  	int n;

  	int c;
  	while(cin >> c)
  	{
  		a[c]++;
  	}

  	for(int i = 0; i < 105; i++)
  		for(int j = 0; j < a[i]; j++)
  			cout << i << ' ';

 	return 0;
}