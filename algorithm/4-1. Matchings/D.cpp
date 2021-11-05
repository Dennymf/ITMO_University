#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>
#include <map>
#include <unordered_set>

#define x first
#define y second

using namespace std;

typedef long long ll;

vector<vector<int>> graf;
vector<int> match;
vector<bool> used;
int n;
double maxspeed;

struct vertex {
    double time;
    pair<int, int> p;

    vertex() = default;

    vertex(const vertex &other) = default;

    vertex &operator=(const vertex &other) = default;

    vertex(int h, int m, pair<int, int> p) {
        this-> p = p;
        time = h * 60 + m;
    }


};

bool operator<(const vertex& a, const vertex& b)
{
    return a.time < b.time;
}

double dist(pair<int, int>& a, pair<int, int>& b){
    return sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y));
}

bool dfs(ll v){
    if (used[v]){
        return false;
    }

    used[v] = true;
    for (auto to : graf[v]){
        if (match[to] == -1){
            match[to] = v;
            return true;
        } else {
            bool res = dfs(match[to]);
            if (res) {
                match[to] = v;
                return true;
            }
        }
    }

    return false;
}

vector<vertex> d;

void createGraph(){
    for (int i = 0; i < n; i++){
        for (int j = i + 1; j < n; j++){
            vertex v = d[i];
            vertex u = d[j];

            if (v.time + dist(v.p, u.p) / maxspeed <= u.time) {
                graf[i].push_back(j);
            }
        }
    }
}

int main(){
    cin >> n >> maxspeed;
    maxspeed /= 60;

    graf.resize(n);
    d.resize(n);
    match.resize(n, -1);

    for (int i = 0; i < n; i++){
        int h, m;
        pair<int, int> p;
        scanf("%d:%d %d %d", &h, &m, &p.x, &p.y);
        
        vertex v(h, m, p);
        d[i] = vertex(v);
    }

    sort(d.begin(), d.end());
    createGraph();

    for (int i = 0; i < n; i++){
        used.clear();
        used.resize(n, false);
        dfs(i);
    }

    vector<pair<int, int>> ans;
    unordered_set<int> left, right;
    map<int, int> leftToRight;

    for (int i = 0; i < n; i++){
        if (match[i] != -1){
            left.insert(match[i]);
            right.insert(i);
            ans.push_back({match[i], i});
            leftToRight[match[i]] = i;
        }
    }

    used.clear();
    used.resize(n);
    int cnt = 0;

    for (auto &iter : ans){
        pair<int, int> t = iter;
        if (used[t.x]){
            continue;
        }

        cnt++;
        while (!used[t.x] && left.count(leftToRight[t.x])){
            t = {leftToRight[t.x], leftToRight[leftToRight[t.x]]};
        }
    }

    cout << n - cnt << endl;

    return 0;
}