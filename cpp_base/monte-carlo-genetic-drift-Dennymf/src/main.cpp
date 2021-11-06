#include "../include/utils.h"

#include <random>
#include <iostream>

double get_next_number()
{
    static auto generator = utils::get_random_generator();
    static auto distribution = utils::get_real_distribution();
    return distribution(generator);
}

int main()
{
    unsigned int number_of_iterations = 500;
    unsigned int population_size = 200;
    double recessive_chance = 0.3;
    double dominant_chance = 0.7;

    std::pair<double, double> answer = utils::get_fixation_extinction_probability(get_next_number,
        number_of_iterations,
        population_size,
        recessive_chance,
        dominant_chance);
    std::cout << "DEBUG print in main:\n";
    std::cout << "fixation probability = " << answer.first << ", extinction probability = " << answer.second << std::endl;
}