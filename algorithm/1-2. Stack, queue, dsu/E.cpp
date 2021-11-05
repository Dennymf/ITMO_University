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

int t = 0;
int a[2000000];

void push(int x)
{
	a[t++] = x;
	return;
}

int pop()
{
	t--;
	return a[t];
}

int sttoint(string s)
{
	int num = 0;
	for (int i = 0; i < s.length(); i++)
		num = num * 10 + s[i] - '0';

	return num;
}
signed main()
{
	ios::sync_with_stdio();
	cin.tie();
	cout.tie();
	
	string s;
	int x, y;

	while (cin >> s)
	{
		if (s == "+")
		{
			x = pop();
			y = pop();
			push(x + y);
		}
		else if (s == "-")
		{
			x = pop();
			y = pop();
			push(y - x);
		}
		else if (s == "*")
		{
			x = pop();
			y = pop();
			push(x * y);
		}
		else
			push(sttoint(s));
	}

	cout << a[0];
}