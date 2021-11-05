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

template<class value_type>

class treap
{
private:
	class node
	{
	public:
		node* x;
		node* y;
		ll s;
		ll val;
		int p;

		node(ll val, int p)
		{
			x = nullptr;
			y = nullptr;
			this->val = val;
			this->p = p;
			s = 1;
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
			return v->s;
	}

	void rsize(node* v)
	{
		v->s = get_size(v->x) + 1 + get_size(v->y);
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

	pair<node*, node*> split(node* v, ll val)
	{
		if (v == nullptr)
		{
			return { nullptr, nullptr };
		}
		else if (v->val < val)
		{
			auto spl = split(v->y, val);
			v->y = spl.x;
			rsize(v);
			return { v, spl.y };
		}
		else
		{
			auto spl = split(v->x, val);
			v->x = spl.y;
			rsize(v);
			return { spl.x, v };
		}
	}

	node* find(node* v, ll val)
	{
		if (v == nullptr)
		{
			return nullptr;
		}
		if (v->val == val)
		{
			return v;
		}
		else if (v->val < val)
		{
			return find(v->y, val);
		}
		else
		{
			return find(v->x, val);
		}
	}

	node* sleft(node* v) 
	{
		if (v == nullptr || v->x == nullptr)
		{
			return v;
		}
		else
		{
			return sleft(v->x);
		}
	}

	node* sright(node* v)
	{
		if (v == nullptr || v->y == nullptr)
		{
			return v;
		}
		else
		{
			return sright(v->y);
		}
	}

	node* get(node* v, int ind)
	{
		if (v == nullptr)
		{
			return nullptr;
		}
		auto lsize = get_size(v->x);
		if (lsize == ind)
		{
			return v;
		}
		else if (lsize < ind)
		{
			return get(v->y, ind - lsize - 1);
		}
		else
		{
			return get(v->x, ind);
		}
	}

public:
	treap()
	{
		r = nullptr;
		rnd = new std::mt19937(std::chrono::steady_clock::now().time_since_epoch().count());
	}

	~treap()
	{
		delete r;
	}

	void insert(ll val)
	{
		if (find(r, val) != nullptr)
		{
			return;
		}
		auto st = split(r, val);
		auto v = new node(val, (*rnd)());
		st.x = merge(st.x, v);
		r = merge(st.x, st.y);
	}

	void erase(ll val) {
		if (find(r, val) == nullptr)
		{
			return;
		}
		auto st1 = split(r, val);
		val++;
		auto st2 = split(st1.y, val);
		delete st2.x;
		r = merge(st1.x, st2.y);
	}

	node* next(ll val)
	{
		val++;
		auto st = split(r, val);
		auto ans = sleft(st.y);
		r = merge(st.x, st.y);
		return ans;
	}

	node* prev(ll val)
	{
		auto st = split(r, val);
		auto ans = sright(st.x);
		r = merge(st.x, st.y);
		return ans;
	}

	node* get(ll ind)
	{
		return get(r, ind);
	}

	ll size()
	{
		return get_size(r);
	}
};

int main()
{
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	ll n;

	cin >> n;
	treap<int> t;
	for (int i = 0; i < n; i++)
	{
		string s;
		ll k;
		cin >> s;
		if (s == "1" || s == "+1")
		{
			cin >> k;
			t.insert(k);
		}
		else if (s == "-1")
		{
			cin >> k;
			t.erase(k);
		}
		else
		{
			cin >> k;
			k = t.size() - k;
			cout << t.get(k)->val << '\n';
		}
	}
	return 0;
}