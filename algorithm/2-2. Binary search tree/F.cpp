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

		node(key_type& key, int p) {
			this->x = nullptr;
			this->y = nullptr;
			this->key = key;
			this->p = p;
			this->size = 1;
		}

		~node() {
			delete x;
			delete y;
		}
	};

	std::mt19937* rnd;
	node* r;

	ll get_size(node* v)
	{
		if (v == nullptr)
			return 0;
		else
			return v->size;
	}

	void rsize(node* v)
	{
		v->size = get_size(v->x) + 1 + get_size(v->y);
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
			a->y = merge(a->y, b);
			rsize(a);
			return a;
		}
		else
		{
			b->x = merge(a, b->x);
			rsize(b);
			return b;
		}
	}

	pair<node*, node*> split(node* v,const key_type& key) {
		if (v == nullptr) {
			return { nullptr, nullptr };
		}
		else if (get_size(v->x) < key) {
			auto st = split(v->y, key - 1 - get_size(v->x));
			v->y = st.first;
			rsize(v);
			return { v, st.second };
		}
		else {
			auto st = split(v->x, key);
			v->x = st.second;
			rsize(v);
			return { st.first, v };
		}
	}
	void print(ostream& stream, node* v, const string& sep, bool sort)
	{
		if (v != nullptr) {
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
		r = nullptr;
		rnd = new std::mt19937(std::chrono::steady_clock::now().time_since_epoch().count());
	}

	~treap() {
		delete r;
	}

	void insert(const key_type& ind, key_type& val) {
		auto st = split(r, ind);
		auto v = new node(val, (*rnd)());
		st.first = merge(st.first, v);
		r = merge(st.first, st.second);
	}

	void erase(const key_type& ind) {
		auto st1 = split(r, ind);
		auto st2 = split(st1.second, 1);
		delete st2.first;
		r = merge(st1.first, st2.second);
	}

	ll size()
	{
		return get_size(r);
	}

	void print(ostream& stream,const string& sep, bool sort) 
	{
		print(stream, r, sep, sort);
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
	for (int i = 0; i < n; i++) {
		ll x;
		cin >> x;
		t.insert(i, x);
	}
	for (int i = 0; i < q; i++) {
		string s;
		int ind;
		cin >> s;
		if (s == "add") {
			ll val;
			cin >> ind >> val;
			t.insert(ind, val);
		}
		else {
			cin >> ind;
			ind--;
			t.erase(ind);
		}
	}
	cout << t.size() << '\n';
	t.print(std::cout, " ", true);

	return 0;
}