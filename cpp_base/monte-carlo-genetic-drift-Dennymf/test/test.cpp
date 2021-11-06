#include "../include/utils.h"

#include <gtest/gtest.h>

TEST(monte_carlo_genetic_drift, population50)
{
    unsigned int number_of_iterations = 3000;
    unsigned int population_size = 50;
    double recessive_chance = 0.1;
    double dominant_chance = 0.9;

    auto generator = utils::get_random_generator();
    auto dist = utils::get_real_distribution();
    auto get_next_num = [&generator, &dist] { return dist(generator); };

    std::pair<double, double> answer = utils::get_fixation_extinction_probability(get_next_num,
        number_of_iterations,
        population_size,
        recessive_chance,
        dominant_chance);

    EXPECT_NEAR(0.1, answer.first, 0.05);
    EXPECT_NEAR(0.9, answer.second, 0.05);
}

TEST(monte_carlo_genetic_drift, population100)
{
    unsigned int number_of_iterations = 500;
    unsigned int population_size = 100;
    double recessive_chance = 0.3;
    double dominant_chance = 0.7;

    auto generator = utils::get_random_generator();
    auto dist = utils::get_real_distribution();
    auto get_next_num = [&generator, &dist] { return dist(generator); };

    std::pair<double, double> answer = utils::get_fixation_extinction_probability(get_next_num,
        number_of_iterations,
        population_size,
        recessive_chance,
        dominant_chance);

    EXPECT_NEAR(0.3, answer.first, 0.1);
    EXPECT_NEAR(0.7, answer.second, 0.1);
}
