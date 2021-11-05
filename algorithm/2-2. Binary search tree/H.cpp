#include <iostream>
#include <cmath>
#include <algorithm>
#include <iomanip>
#include <vector>
#include <set>
#include <map>
#include <deque>
#include <stack>
#include <unordered_map>
#include <random>
#include <chrono>

using namespace std;

typedef long long ll;
typedef long double ld;
typedef pair<ll, ll> pl;
typedef pair<ld, ld> pd;

const ll N = 5 * 1e6 + 100;
const ll M = 1e4 + 100;
const int INF = 1e9 + 123;
using namespace std;

#define x first
#define y second

template<class key_type>

class treap
{
private:
	class node {
	public:
		node* x;
		node* y;
		key_type key;
		int p;
		int size;
		bool rev;

		node(const	key_type& key, int p) {
			this->x = nullptr;
			this->y = nullptr;
			this->key = key;
			this->p = p;
			this->size = 1;
			rev = 0;
		}

		~node() {
			delete x;
			delete y;
		}
	};

	std::mt19937* rnd;
	node* rt;

	ll get_size(node* v)
	{
		if (v == nullptr)
			return 0;
		else
			return v->size;
	}

	node* merge(node* a, node* b)
	{
		if (a == nullptr)
		{
			return b;
		}
		else if (b == nullptr)
		{
			return a;
		}
		else if (a->p < b->p)
		{
			recalc(a);
			a->y = merge(a->y, b);
			recalc(a);
			return a;
		}
		else
		{
			recalc(b);
			b->x = merge(a, b->x);
			recalc(b);
			return b;
		}
	}

	void recalc_size(node* v) {
		v->size = get_size(v->x) + 1 + get_size(v->y);
	}

	void recalc(node* v) {
		recalc_size(v);
		if (v->rev) {
			swap(v->x, v->y);
			reverse(v->x);
			reverse(v->y);
			v->rev = false;
		}
	}

	pair<node*, node*> split(node* v,const key_type& key) {
		if (v == nullptr) {
			return { nullptr, nullptr };
		}
		recalc(v);
		if (get_size(v->x) < key) {
			auto st = split(v->y, key - 1 - get_size(v->x));
			v->y = st.first;
			recalc(v);
			return { v, st.second };
		}
		else
		{
			auto st = split(v->x, key);
			v->x = st.second;
			recalc(v);
			return { st.first, v };
		}
	}

	void reverse(node* v) {
		if (v == nullptr) {
			return;
		}
		v->rev ^= 1;
	}

	void print(ostream& stream, node* v, const string& sep, bool sort)
	{
		if (v != nullptr) {
			recalc(v);
			if (sort) {
				print(stream, v->x, sep, sort);
				stream << v->key << sep;
				print(stream, v->y, sep, sort);
			}
			else {
				stream << v->key << sep;
				print(stream, v->x, sep, sort);
				print(stream, v->y, sep, sort);
			}
		}
	}

public:
	treap() {
		rt = nullptr;
		rnd = new std::mt19937(std::chrono::steady_clock::now().time_since_epoch().count());
	}

	~treap() {
		delete rt;
	}

	void insert(const key_type& ind, const key_type& val) {
		auto st = split(rt, ind);
		auto v = new node(val, (*rnd)());
		st.first = merge(st.first, v);
		rt = merge(st.first, st.second);
	}

	void reverse(ll l, ll r) {
		auto st1 = split(rt, r);
		auto st2 = split(st1.first, l);
		reverse(st2.second);
		st1.first = merge(st2.first, st2.second);
		rt = merge(st1.first, st1.second);
	}

	ll size()
	{
		return get_size(rt);
	}

	void print(ostream& stream,const string& sep, bool sort) 
	{
		print(stream, rt, sep, sort);
	}
};

int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	int n, q;

	cin >> n >> q;
	treap<ll> t;
	for (ll i = 0; i < n; i++) {
		t.insert(i, i + 1);
	}
	for (ll i = 0; i < q; i++) {
		ll l, r;
		cin >> l >> r;
		l--;
		t.reverse(l, r);
	}
	t.print(cout, " ", true);

	return 0;
}