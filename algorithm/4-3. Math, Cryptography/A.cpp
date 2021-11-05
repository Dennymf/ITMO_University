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

#define x first
#define y second
#define sq(a) ((a) * (a))

typedef long long ll;
typedef long double ld;

const ll N = 2e5 + 100;
using namespace std;

vector<int> primes;
vector<bool> prime;

void all_prime()
{
    for (ll i = 2; i <= N; i++)
    {
        if (prime[i])
        {
            primes.push_back(i);
            if (i * i > N)
                continue;

            for (ll j = i * i; j <= N; j += i)
                prime[j] = false;
        }
    }
}

bool is_prime(int t)
{

    for (int i = 2; i <= sqrt(t); i++)
        if (t % i == 0)
            return false;

    return true;
}

int main()
{
    ios::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);
    
    int n;

    cin >> n;
    prime.resize(N + 1, true);
    
    all_prime();

    int index = 0;
    while (n > 1)
    {
        if (n % primes[index] == 0)
        {
            cout << primes[index] << " ";
            n /= primes[index];
        }
        else
        {
            index++;
            if (is_prime(n))
            {
                cout << n;
                return 0;
            }
        }
    }
}