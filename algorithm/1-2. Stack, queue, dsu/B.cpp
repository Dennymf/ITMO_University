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

int a[2000000];
int t = 0, n;
void push(int x)
{
	a[t++] = x;
	return;
}

void pop()
{
	t--;
	return;
}

signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	int ans = 0, x;
	cin >> n;

	for (int i = 0; i < n; i++)
	{
		bool b = 0;
		cin >> x;

		while (t > 1 && a[t - 1] == x && a[t - 2] == x)
		{
			int k = x;
			int sum = 1;
			b = 0;
			int g = t;
			while (a[g - 1] == k && g)
				sum++, g--;
				
			i++;
			if (i >= n)
			{
				push(x);
				b = 1;
				break;
			}
			for (int e = 0; e < sum - 1; e++)
				pop();
			cin >> x;
			while (x == k)
			{
				sum++;
				i++;
				if (i >= n)
					break;
				cin >> x;
			}
			ans += sum;
		}
		if(!b)
			push(x);
	}
	if (t > 2 && a[t - 1] == a[t - 3] && a[t - 2] == a[t - 1])
	{
		int k = a[t - 1];
		while (a[t - 1] == k && t)
			pop(), ans++;
	}
	cout << ans;
}
