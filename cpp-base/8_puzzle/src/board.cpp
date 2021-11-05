#include "board.h"
#include <algorithm>
#include <random>
#include <iostream>

Board Board::create_goal(unsigned size) {
    std::vector<std::vector<unsigned>> board_goal;
    board_goal.resize(size, std::vector<unsigned>(size));
    size_t number = size * size;

    for(size_t i = 0; i < number; i++)
        board_goal[i / size][i % size] = (i + 1) % number;

    return Board(board_goal);
}

Board::Board(const std::vector<std::vector<unsigned>> &other): b(other){
    _size = other.size();
    find_zero();
}

Board::Board(const unsigned size) {
    b.resize(size, std::vector<unsigned>(size));
    std::vector<unsigned> some_board;
    some_board.resize(size * size);
    _size = size;
    for (unsigned i = 0; i < size * size; i++) {
        some_board[i] = i;
    }
    std::shuffle(some_board.begin(), some_board.end(), std::mt19937(std::random_device()()));
    for (unsigned i = 0; i < size * size; i++) {
        unsigned x = i / size;
        unsigned y = i % size;
        b[x][y] = some_board[i];
        if (!b[x][y]) {
            zero = {x, y};
        }
    }
}

size_t Board::size() const {
    return b.size();
}

bool Board::is_goal() const {
	return hamming() == 0;
}

size_t Board::hamming() const {
    size_t dist_hamming = 0;
    size_t t = 0;
    size_t max = _size * _size - 1;
    for (size_t i = 0; i < _size; i++) {
        for (size_t j = 0; j < _size; j++) {
            if(t < max)
            {
                t++;
            } else {
                t = 0;
            }
            if(b[i][j] != t)
                dist_hamming++;
        }
    }

    return dist_hamming;
}

size_t Board::manhattan() const {
    size_t dist_manhattan = 0;

    for (size_t i = 0; i < _size; i++) {
        for (size_t j = 0; j < _size; j++) {
            int need_x = static_cast<int>((b[i][j] - 1) / _size);
            int need_y = static_cast<int>((b[i][j] - 1) % _size);

            if (b[i][j]) {
                dist_manhattan += abs(static_cast<int>(i) - need_x) + abs(static_cast<int>(j) - need_y);
            }
        }
    }

    return dist_manhattan;
}

std::string Board::to_string() const {
    std::string result;

    for (size_t i = 0; i < _size; i++) {
        for (size_t j = 0; j < _size; j++) {
            result += std::to_string(b[i][j]) + ' ';
        }

        result.pop_back();
        result.push_back('\n');
    }

    return result;
}

bool Board::is_solvable() const {
    std::vector<unsigned> v(_size * _size);
    size_t cnt_inv = 0;

    if(is_goal())
        return true;

    for (size_t i = 0; i < _size; i++) {
        for (size_t j = 0; j < _size; j++) {
            v[i * _size + j] = b[i][j];
        }
    }

    for (size_t i = 0; i < v.size(); i++) {
        if (v[i]) {
            for (size_t j = 0; j < i; j++) {
                if (v[j] > v[i]) {
                    cnt_inv++;
                }
            }
        }
    }

    if(!(_size % 2))
    {
        for (size_t i = 0; i < v.size(); i++) {
            if (!v[i]) {
                cnt_inv += 1 + i / _size;
            }
        }
    }
    return !(cnt_inv & 1);
}

const std::vector<unsigned> &Board::operator[](size_t index) const {
    return b[index];
}

std::pair<unsigned, unsigned> Board::get_zero() const {
    return zero;
}

std::vector<std::vector<unsigned>> Board::get_board() const {
    return b;
}

void Board::find_zero() {
    for (unsigned i = 0; i < _size; i++) {
        for (unsigned j = 0; j < _size; j++) {
            if (!b[i][j]) {
                zero = {i, j};
                break;
            }
        }
    }
}

std::ostream &operator<<(std::ostream &out, const Board &b) {
    out << b.to_string();
    return out;
}

bool operator==(const Board &a, const Board &b) {
    return a.b == b.b;
}

bool operator!=(const Board &a, const Board &b) {
    return a.b != b.b;
}

bool operator <(const Board &a, const Board &b) {
    return a.b < b.b;
}