#pragma once

#include <random>
#include <utility>
#include <functional>

namespace utils
{
    /**
     * Функция подсчитывает веростность фиксации или исчезновения рецессивного признака
     * @param generator генератор случайных вещественных чисел на отрезке [0,1)
     * @param number_of_iterations количество итераций моделирования (где каждая итерация заканчивается тем, что один признак фиксируется)
     * @param population_size колчиство особей в популяции
     * @param recessive_chance начальная частота встречаемости рецессивной аллели
     * @param dominant_chance начальная частота встречаемости доминантной аллели
     * @return (fixation_p, extenction_p) где fixation_p вероятность фиксации рецессивного признака
     *                                         extenction_p вероятность исчезновения рецессивного признака
     */
    std::pair<double, double> get_fixation_extinction_probability(std::function<double()> generator,
                                                                  unsigned int number_of_iterations,
                                                                  unsigned int population_size,
                                                                  double recessive_chance,
                                                                  double dominant_chance);

    /**
     * Возвращает равномерное распределение для чисел в интервале  [0, 1)
     */
    std::uniform_real_distribution<> get_real_distribution();

    /**
     * Возвращет генератор случайных чисел
     */
    std::mt19937_64 get_random_generator();
}