#include "solver.h"
#include <queue>
#include <algorithm>

Solver::Solver(const Board &b) {
    start_board = b;
    transition = {{-1, 0},
                  {0,  1},
                  {1,  0},
                  {0,  -1}};
    board_goal = Board::create_goal(b.size());
    solve();
}

void Solver::solve() {
    if (!start_board.is_solvable()) {
        return;
    }

    auto comparator = [](const Board &left, const Board &right)
    {
        return left.hamming() + left.manhattan() > right.hamming() + right.manhattan();
    };

    std::priority_queue<Board, std::vector<Board>, decltype(comparator)> q(comparator);
    std::unordered_set<Board> used;
    std::unordered_map<Board, size_t> distance;

    q.push(start_board);
    distance[start_board] = 0;
    while (!q.empty()) {
        Board current = q.top();
        if (current.is_goal()) {
            get_parents();
            return;
        }
        q.pop();
        used.insert(current);

        std::pair<size_t, size_t> zero = current.get_zero();
        for (auto [dx, dy] : transition) {
            size_t new_x = zero.first + dx;
            size_t new_y = zero.second + dy;
            Board changed_board = current;

            if (check(new_x, new_y)) {
                auto tmp = changed_board.get_board();
                std::swap(tmp[zero.first][zero.second], tmp[new_x][new_y]);
                changed_board = Board(tmp);
                bool already_used = used.count(changed_board);

                size_t cur_dist = distance[current] + changed_board.hamming() + changed_board.manhattan();

                if (already_used && cur_dist >= distance[changed_board]) {
                    continue;
                }

                parent[changed_board] = current;
                distance[changed_board] = cur_dist;

                if (!already_used) {
                    q.push(changed_board);
                }

            }
        }
    }
}

void Solver::get_parents() {
    Board v = board_goal;
    while (v != start_board) {
        boards.emplace_back(v);
        v = parent[v];
    }
    boards.emplace_back(v);
    reverse(boards.begin(), boards.end());
}

bool Solver::check(size_t x, size_t y) const {
    int xx = x;
    int yy = y;
    return xx >= 0 && x < start_board.size() && yy >= 0 && y < start_board.size();
}

size_t Solver::moves() const {
    if(boards.empty())
        return 0;
    return boards.size() - 1;
}