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

void file()
{
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);     
}   

int n, t = 0, h = 0;
int a[200000];
int ind[2000000];
 
inline void push(int x)
{
	a[t++] = x;
	return;
}
 
inline void pop()
{
	h++;
	return;
}

inline void pop_back()
{
	t--;
	return;
}
signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	cin >> n;
 
 	int c, x;

	for(int i = 0; i < n; i++)
	{
		cin >> c;
		if(c == 1)
		{
			cin >> x;
			ind[x] = t;
			push(x);
		}
		else if(c == 2)
			pop();
		else if(c == 3)
		{
			pop_back();
		}
		else if(c == 4)
		{
			cin >> x;
			cout << ind[x] - h << '\n';
		}
		else if(c == 5)
			cout << a[h] << '\n';
	}
}