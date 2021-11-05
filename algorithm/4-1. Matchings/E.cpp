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

const int dx[] = {0, 1, 0, -1};
const int dy[] = {1, 0, -1, 0};

vector<vector<int>> graf;
vector<int> match;
vector<bool> used;
int n, m;
vector<vector<bool>> board;

bool free(pair<int, int> p){
    return p.x >= 0 && p.x < n && p.y >= 0 && p.y < m && !board[p.x][p.y];
}

bool dfs(int v){
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

int main() {
    int a, b;
    cin >> n >> m >> a >> b;

    vector<vector<int>> num;

    board.resize(n);
    num.resize(n);
    int cnt = 0;
    int cntFree = 0;

    for (int i = 0; i < n; i++){
        string s;
        cin >> s;

        board[i].resize(s.size());
        num[i].resize(s.size());

        for (int j = 0; j < s.size(); j++){
            num[i][j] = cnt;
            cnt++;
            if (s[j] == '.'){
                board[i][j] = 1;
            } else {
                cntFree++;
            }
        }
    }

    graf.resize(cnt);
    match.resize(cnt, -1);
    used.resize(cnt);

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (free({i, j}) && (i + j) % 2) {
                for (int k = 0; k < 4; k++) {
                    int x = i + dx[k];
                    int y = j + dy[k];

                    if (free({x, y})) {
                        graf[num[i][j]].push_back(num[x][y]);
                    }
                }
            }
        }
    }

    for (int i = 0; i < cnt; i++){
        used.clear();
        used.resize(cnt, false);
        dfs(i);
    }

    vector<pair<int, int>> ans;

    for (int i = 0; i < cnt; i++){
        if (match[i] != -1){
            ans.push_back({match[i], i});
        }
    }

    int sizee = ans.size();

    cout << min(cntFree * b, sizee * a + (cntFree - sizee * 2) * b);

    return 0;
}