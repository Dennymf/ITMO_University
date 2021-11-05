#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <algorithm>

using namespace std;

#define x first
#define y second
#define sq(a) ((a) * (a))

typedef long long ll;
typedef long double ld;
typedef unsigned long ul;
const ll N = 4e6 + 10;
const ll INF = 1e18;

struct Node {
	ll value = 0, d = INF;
};

struct query {
	ll left, right, value;
};

query q[N];
Node t[N];

void push(int v) {
	if (t[v].d == INF) {
		return;
	}

	t[v * 2 + 1].d = t[v * 2 + 2].d = t[v].d;
	t[v].value = t[v].d;
	t[v].d = INF;
}


void update(int tl, int tr, ll x, int v, int l, int r) {
	push(v);

	if (l >= tl && r <= tr) {
		t[v].d = x;
		push(v);
		return;
	}

	if (r < tl || l > tr) {
		return;
	}

	int mid = (l + r) / 2;

	update(tl, tr, x, v * 2 + 1, l, mid);
	update(tl, tr, x, v * 2 + 2, mid + 1, r);

	t[v].value = min(t[v * 2 + 1].value, t[v * 2 + 2].value);
}

bool cmp(const query& a, const query& b) {
	if (a.value == b.value && a.left == b.left && a.right < b.right) {
		return true;
	}

	if (a.value == b.value && a.left < b.left) {
		return true;
	}

	return a.value < b.value;
}

ll min(int v, int l, int r, int tl, int tr) {
	push(v);

	if (l >= tl && r <= tr) {
		return t[v].value;
	}

	if (r < tl || l > tr) {
		return INF;
	}

	int mid = (l + r) / 2;
	push(v);
	return min(min(v * 2 + 1, l, mid, tl, tr), min(v * 2 + 2, mid + 1, r, tl, tr));
}

int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	freopen("rmq.in", "r", stdin);
	freopen("rmq.out", "w", stdout);


	ll n, m;
	cin >> n >> m;

	for (int i = 0; i < m; i++) {
		ll left, right, value;
		cin >> left >> right >> value;
		left--, right--;

		q[i].value = value;
		q[i].left = left;
		q[i].right = right;
	}

	sort(q, q + m, cmp);

	for (int i = 0; i < m; i++) {
		update(q[i].left, q[i].right, q[i].value, 0, 0, n - 1);
	}

	for (int i = 0; i < m; i++) {
		if (q[i].value != min(0, 0, n - 1, q[i].left, q[i].right)) {
			return cout << "inconsistent", 0;
		}
	}


	cout << "consistent" << endl;
	for (int i = 0; i < n; i++) {
		cout << min(0, 0, n - 1, i, i) << " ";
	}

	return 0;
}