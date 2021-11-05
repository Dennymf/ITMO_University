#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>
#include <set>

#define x first
#define y second

typedef long long ll;

using namespace std;

vector<vector<int>> graf, rGraf, org;
vector<int> match;
vector<bool> used, visit;
int n, m;
double maxspeed;
vector<vector<bool>> board;

void second_dfs(int v){
    if (visit[v]){
        return;
    }

    visit[v] = true;

    for (auto &to : org[v]){
        second_dfs(to);
    }
}

bool dfs(int v) {
    if (used[v]) {
        return false;
    }

    used[v] = true;

    for (auto to : rGraf[v]){
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

int main(){
    int k;
    cin >> k;

    for(int t = 0; t < k; t++){
        int n, m;
        cin >> n >> m;

        vector<vector<int>> num;

        graf.clear();
        graf.resize(n);
        rGraf.clear();
        rGraf.resize(n);
        match.clear();
        match.resize(m, -1);
        vector<vector<bool>> rev;
        rev.resize(n, vector<bool>(m, true));

        int to;

        for (int i = 0; i < n; i++){
            cin >> to;

            while (to){
                graf[i].push_back(to - 1);
                rev[i][to - 1] = false;

                cin >> to;
            }
        }

        for (int i = 0; i < n; i++){
            for (int j = 0; j < m; j++){
                if (rev[i][j]){
                    rGraf[i].push_back(j);
                }
            }
        }

        for (int i = 0; i < n; i++){
            used.clear();
            used.resize(n, false);
            dfs(i);
        }

        vector<pair<int, int>> p;
        vector<vector<bool>> matchMatrix(n, vector<bool>(m, false));

        set<int> matched;

        for (int i = 0; i < m; i++){
            if (match[i] != -1){
                p.push_back({match[i], i});
                matchMatrix[match[i]][i] = true;
                matched.insert(match[i]);
            }
        }

        org.clear();
        org.resize(n + m);

        for (int i = 0; i < n; i++){
            for (int j = 0; j < rGraf[i].size(); j++){
                if (matchMatrix[i][rGraf[i][j]]){
                    org[rGraf[i][j] + n].push_back(i);
                } else {
                    org[i].push_back(rGraf[i][j] + n);
                }
            }
        }

        visit.clear();
        visit.resize(n + m);

        for (int i = 0; i < n; i++){
            if (!matched.count(i)){
                second_dfs(i);
            }
        }

        set<int> cover;

        for (int i = 0; i < n + m; i++){
            if (i < n){
                if (!visit[i]){
                    cover.insert(i);
                }
            } else {
                if (visit[i]){
                    cover.insert(i);
                }
            }
        }

        set<int> boys, girls;
        for (int i = 0; i < n + m; i++){
            if (!cover.count(i)){
                if (i < n){
                    boys.insert(i + 1);
                } else {
                    girls.insert(i - n + 1);
                }
            }
        }


        cout << girls.size() + boys.size() << endl << boys.size() << " " << girls.size() << endl;
        for (auto &iter : boys) {
            cout << iter << " ";
        }
        cout << endl;

        for (auto &iter : girls){
            cout << iter << " ";
        }
        cout << endl;
    }

    return 0;
}