#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>
#include <set>

#define x first
#define y second

typedef long long ll;

using namespace std;

vector<vector<int>> grafLeft, grafRight;
vector<int> matchLeft, matchRight;
vector<bool> used;
vector<pair<int, int>> weightLeft, weightRight;
int edgNum[7000][7000];

bool dfs(int v, vector<vector<int>> &graf, vector<int> &match){
    if (used[v]){
        return false;
    }

    used[v] = true;
    for (auto to : graf[v]){
        if (match[to] == -1){
            match[to] = v;
            return true;
        } else {
            bool res = dfs(match[to], graf, match);
            if (res){
                match[to] = v;
                return true;
            }
        }
    }

    return false;
}


int main(){
    ll n, m ,e;

    cin >> n >> m >> e;

    grafLeft.resize(n);
    grafRight.resize(m);

    weightLeft.resize(n);
    weightRight.resize(m);

    vector<int> weightL(n), weightR(m);

    matchLeft.resize(m, -1);
    matchRight.resize(n, -1);

    int x;

    for(int i = 0; i < n; i++){
        cin >> x;

        weightLeft[i] = {x, i};
        weightL[i] = x;
    }
    for(int i = 0; i < m; i++){
        cin >> x;

        weightRight[i] = {x, i};
        weightR[i] = x;
    }
    for(int i = 0; i < e; i++){
        int l, r;
        cin >> l >> r;
        l--, r--;

        edgNum[l][r] = i;
        grafLeft[l].push_back(r);
        grafRight[r].push_back(l);
    }

    sort(weightLeft.rbegin(), weightLeft.rend());

    for (int i = 0; i < n; i++){
        used.clear();
        used.resize(n, false);
        dfs(weightLeft[i].y, grafLeft, matchLeft);
    }

    set<int> leftInMatch;

    for (int i = 0; i < m; i++){
        if (matchLeft[i] != -1){
            leftInMatch.insert(matchLeft[i]);
        }
    }

    sort(weightRight.rbegin(), weightRight.rend());

    for (int i = 0; i < m; i++){
        used.clear();
        used.resize(m, false);
        dfs(weightRight[i].y, grafRight, matchRight);
    }

    set<int> rightInMatch;

    for (int i = 0; i < n; i++){
        if (matchRight[i] != -1){
            rightInMatch.insert(matchRight[i]);
        }
    }

    vector<vector<int>> newGraf(n);

    for (int i = 0; i < n; i++){
        for (int j = 0; j < grafLeft[i].size(); j++){
            if (leftInMatch.count(i) && rightInMatch.count(grafLeft[i][j])){
                newGraf[i].push_back(grafLeft[i][j]);
            }
        }
    }

    vector<int> match(m, -1);
    vector<pair<int, int>> weight(n);

    for (int i = 0; i < n; i++){
        if (leftInMatch.count(i)){
            weight[i] = {weightL[i], i};
        }
    }

    sort(weight.rbegin(), weight.rend());

    for (int i = 0; i < n; i++){
        used.clear();
        used.resize(n, false);
        dfs(weight[i].y, newGraf, match);
    }

    ll maxWeight = 0;
    vector<int> maxMatchEdgNums;

    for (int i = 0; i < m; i++){
        if (match[i] != -1){
            maxWeight += weightL[match[i]];
            maxWeight += weightR[i];
            maxMatchEdgNums.push_back(edgNum[match[i]][i]);
        }
    }

    cout << maxWeight << endl << maxMatchEdgNums.size() << endl;

    for (auto iter : maxMatchEdgNums){
        cout << iter + 1 << " ";
    }

}