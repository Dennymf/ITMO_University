#pragma once

#include <vector>
#include <string>

struct Board {
    static Board create_goal(unsigned  size);

    Board() = default;

    Board(const Board & other) = default;

    Board & operator = (const Board & other) = default;

    explicit Board(unsigned size);

    explicit Board(const std::vector<std::vector<unsigned>> & data);

    size_t size() const;

    bool is_goal() const;

    size_t hamming() const;

    size_t manhattan() const;

    std::string to_string() const;

    bool is_solvable() const;

    friend bool operator!=(const Board &a, const Board &b);

    friend bool operator==(const Board &a, const Board &b);

    friend bool operator<(const Board &a, const Board &b);

    friend std::ostream &operator<<(std::ostream &out, const Board &b);

    const std::vector<unsigned> &operator[](size_t index) const;

    std::pair<unsigned, unsigned> get_zero() const;

    std::vector<std::vector<unsigned>> get_board() const;

private:
    std::vector<std::vector<unsigned>> b;
    std::pair<unsigned, unsigned> zero;
    void find_zero();
    size_t _size = 0;
};

namespace std {
    template <>
    struct hash<Board> : hash<std::string> {
        using argument_type = Board;
        using result_type = unsigned;
        unsigned operator() (const Board & b) const noexcept {
            return hash<std::string>::operator()(b.to_string());
        }
    };
}