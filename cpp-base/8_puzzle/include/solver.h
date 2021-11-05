#pragma once

#include "board.h"
#include<unordered_map>
#include <unordered_set>
#include <queue>

struct Solver {
    explicit Solver(const Board &b);

    Solver(const Solver &s) = default;

    Solver &operator=(const Solver &other) = default;

    auto begin() const
    {
        return boards.begin();
    }

    auto end() const
    {
        return boards.end();
    }

    size_t moves() const;

private:

    std::vector<std::pair<char, char>> transition;
    Board start_board;
    Board board_goal;
    std::vector<Board> boards;
    std::unordered_map<Board, Board> parent;

    void solve();

    void get_parents();

    bool check(size_t x, size_t y) const;
};
