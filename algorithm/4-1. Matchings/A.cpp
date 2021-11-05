#include <iostream>
#include <vector>

#define x first
#define y second

typedef long long ll;

using namespace std;

vector<ll> match;
vector<vector<ll>> graf;
vector<pair<ll, ll>> ans;
vector<bool> used;

bool dfs(ll v){
    if(used[v]){
        return false;
    }

    used[v] = true;
    for(auto to : graf[v]){
        if(match[to] == -1){
            match[to] = v;
            return true;
        } else {
            if(dfs(match[to])) {
                match[to] = v;
                return true;
            }
        }
    }

    return false;
}

int main(){
    ll n, m;

    cin >> n >> m;

    graf.resize(n);
    match.resize(m, -1);

    ll x;

    for(int i = 0; i < n; i++){
        cin >> x;

        while(x){
            graf[i].push_back(x - 1);
            cin >> x;
        }
    }

    for(int i = 0; i < n; i++){
        used.clear();
        used.resize(n, false);
        dfs(i);
    }

    for(int i = 0; i < m; i++){
        if(match[i] != -1){
            ans.push_back({match[i] + 1, i + 1});
        }
    }

    cout << ans.size() << endl;

    for(auto it : ans){
        cout << it.x << " " << it.y << endl;
    }
}