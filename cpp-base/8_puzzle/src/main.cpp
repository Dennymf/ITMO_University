#include <iostream>
#include "solver.h"
#include <vector>
#include <random>

using namespace std;

int main()
{
    Board board;
    Solver solver(board);
    std::cout << solver.moves() << std::endl;
    
	/*
    for (const auto move : solver) {
        std::cout << move << std::endl;
    }
    */
}