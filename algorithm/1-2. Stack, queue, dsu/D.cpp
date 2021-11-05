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

int n;

struct myDeque {
	vector <ll> a = vector <ll>(10000500);
	ll h = 10000500 / 2, t = 10000500 / 2;

	void push_back(ll v)
	{
		a[t++] = v;
	}

	void push_front(ll v)
	{
		a[--h] = v;
	}

	ll pop_back()
	{
		return a[--t];
	}

	ll pop_front()
	{
		return a[h++];
	}

	ll size()
	{
		return t - h;
	}
};
signed main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	cin >> n;

	int x;
	char c;
	
	myDeque deq1, deq2;

	for (int i = 0; i < n; i++)
	{
		cin >> c;
		if (c != '*')
		{
			if (c == '+')
			{
				cin >> x;
				deq2.push_back(x);
			}
			else
				cout << deq1.pop_front() << '\n';
			if (deq2.size() > deq1.size())
				deq1.push_back(deq2.pop_front());
		}
		else if(c == '*')
		{
			cin >> x;
			deq1.push_back(x);
			if (deq1.size() - deq2.size() > 1)
				deq2.push_front(deq1.pop_back());
		}
	}
}
