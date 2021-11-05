#include <iostream>
#include <iomanip>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <vector>
#include <map>
#include <unordered_set>
#include <string>
#include <queue>
#include <set>
#include <cstdlib>
#include <cstdio>
#include <complex>

#define sq(a) ((a) * (a))

using namespace std;

typedef long long ll;
typedef long double ld;
typedef complex<double> base;

const long double PI = acos(-1);

void fft(vector<base>& a, bool invert)
{
	int n = a.size();
	if (n == 1)
		return;

	vector<base> a0(n / 2), a1(n / 2);
	for (int i = 0, j = 0; i < n; i += 2, ++j) {
		a0[j] = a[i];
		a1[j] = a[i + 1];
	}

	fft(a0, invert);
	fft(a1, invert);

	ld angle = 2 * PI / n;
	if (invert)
		angle = -angle;
	
	base w(1), wn(cos(angle), sin(angle));
	
	for (int i = 0; i < n / 2; i++)
	{
		a[i] = a0[i] + w * a1[i];
		a[i + n / 2] = a0[i] - w * a1[i];
		if (invert)
		{
			a[i] /= 2;
			a[i + n / 2] /= 2;
		}
		w *= wn;
	}
}

vector<int> multiply(const vector<int>& a, const vector<int>& b)
{
	vector<base> f_a(a.begin(), a.end());
	vector<base> f_b(b.begin(), b.end());

	vector<int> res;

	int n = 1;
	while (n < max(a.size(), b.size()))
	{
		n <<= 1;
	}
	n <<= 1;

	f_a.resize(n);
	f_b.resize(n);

	fft(f_a, false);
	fft(f_b, false);

	for (int i = 0; i < n; i++)
		f_a[i] *= f_b[i];

	fft(f_a, true);

	res.resize(n);
	for (int i = 0; i < n; i++)
		res[i] = int(f_a[i].real() + 0.5);

	return res;
}

int main(){
	ios::sync_with_stdio(0);
	cin.tie(0);
	cout.tie(0);

	int n;
	vector<int> a, b, ans;
	cin >> n;
	a.resize(n + 1);
	b.resize(n + 1);

	for (int i = 0; i <= n; i++)
		cin >> a[i];

	for (int i = 0; i <= n; i++)
		cin >> b[i];

	ans = multiply(a, b);

	for (int i = 0; i < 2 * n + 1; i++)
		cout << ans[i] << " ";

	return 0;
}