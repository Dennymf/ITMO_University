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

ll t = 0, n;
ll heap[200000];

void insert(int x)
{
	heap[t++] = x;
	int i = t - 1;
	while (i > 0 && heap[i] > heap[(i - 1) / 2])
		swap(heap[i], heap[(i - 1) / 2]), i = (i - 1) / 2;
}

ll extract()
{
	int value = heap[0];
	swap(heap[0], heap[t - 1]), t--;
	int i = 0;
	while(1)
	{
		int j = i;
		if(2 * i + 1 < t && heap[2 * i + 1] > heap[j])
			j = 2 * i + 1;
		if(2 * i + 2 < t && heap[2 * i + 2] > heap[j])
			j = 2 * i + 2;
		if(i == j)
			break;
		else
			swap(heap[i], heap[j]), i = j;
	}
	return value;
}
signed main()
{
    ios::sync_with_stdio();
    cin.tie();
    cout.tie();

 	cin >> n;

 	int c, x;
 	for(int i = 0; i < n; i++)
 	{
 		cin >> c;
 		if(c == 0)
 		{
 			cin >> x;
 			insert(x);
 		}
 		else if(c == 1)
 			cout << extract() << endl;
 	}
 	return 0;
}