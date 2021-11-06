
#include "../include/utils.h"
#include <iostream>
#include <cmath>

namespace utils
{
    std::uniform_real_distribution<> get_real_distribution()
    {
        // produces random floating-point values uniformly distributed on the interval
        return std::uniform_real_distribution<>(0.0, 1.0);
    }

    std::mt19937_64 get_random_generator()
    {
        // non-deterministic random number generator using hardware entropy source
        std::random_device random_device;
        // seeding Mersenne Twister with a non-deterministic value
        return std::mt19937_64(random_device());
    }
	
	char random_parent_gen(std::pair<char, char> first_parent, std::pair<char, char> second_parent, double gener)
	{
		if (gener <= 0.25)
			return first_parent.first;
		else if (gener <= 0.5)
			return first_parent.second;
		else if (gener <= 0.75)
			return second_parent.first;
		else
			return second_parent.second;
	}

    std::pair<double, double> get_fixation_extinction_probability(std::function<double()> generator,
                                                                  unsigned int number_of_iterations,
                                                                  unsigned int population_size,
                                                                  double recessive_chance,
                                                                  double dominant_chance)
    {
		int fixation_recessive_gen = 0, fixation_dominant_gen = 0;

		for (int iteration = 0; iteration < number_of_iterations; iteration++)	
		{
			int all_gen = population_size * 2;
			bool fixed = 0;
			std::vector<std::pair<char, char> > gen(population_size);
			
			for (int index = 0; index < gen.size(); index++)
			{
				gen[index].first = (generator() <= dominant_chance) ? 'B' : 'A';
				gen[index].second = (generator() <= dominant_chance) ? 'B' : 'A';
			}

			while (!fixed)
			{
				std::vector<std::pair<char, char> > child_new_generation(population_size);
				int current_recessive_gen = 0, current_dominant_gen = 0;

				for (int i = 0; i < population_size; i++)
				{
					std::pair<char, char> child;
					int first_parent = generator() * population_size;
					int second_parent = generator() * population_size;

					while (first_parent == second_parent)
					{
						second_parent = generator() * population_size;
					}
					
					child.first = random_parent_gen(gen[first_parent], gen[second_parent], generator());
					child.second = random_parent_gen(gen[first_parent], gen[second_parent], generator());

					(child.first == 'A') ? current_recessive_gen++ : current_dominant_gen++;
					(child.second == 'A') ? current_recessive_gen++ : current_dominant_gen++;

					if (current_recessive_gen == all_gen)
					{
						fixed = 1;
						fixation_recessive_gen++;
					}
					else if (current_dominant_gen == all_gen)
					{
						fixed = 1;
						fixation_dominant_gen++;
					}
					child_new_generation[i] = child;
				}

				gen = child_new_generation;
			}
		}

		double answer = static_cast<double>(fixation_recessive_gen) / static_cast<double>(number_of_iterations);

		return {answer, 1.0 - answer};
    }
}