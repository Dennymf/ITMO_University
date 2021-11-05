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

int n, t = 0;
int a[200000];
vector<string> v;
 
inline void push(int x)
{
	a[t++] = x;
	v.push_back("push");
	return;
}
 
inline void pop()
{
	t--;
	v.push_back("pop");
	return;
}

signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);
	
	cin >> n;

	int k = 1, x;

	for(int i = 0; i < n; i++)
	{
		cin >> x;
		push(x);
		while(a[t - 1] == k)
			pop(), k++;
	}
	if(t > 0)
		cout << "impossible\n";
	else
		for(int i = 0; i < v.size(); i++)
			cout << v[i] << '\n';
}