![Monte Carlo genetic drift CI](https://github.com/itiviti-cpp-master/monte-carlo-genetic-drift/workflows/Monte%20Carlo%20genetic%20drift%20CI/badge.svg)
# Генетический дрифт

## Суть явления

Несмотря на то, что самым известным механизмом эволюции является естественный отбор, это далеко не единственный механизм - и, возможно, не самый влиятельный.

Среди других механизмов можно отметить т.н. "генетический дрифт". Это случайный процесс, ведущий к изменению частот встречаемости в популяции аллелей одного гена, не связанный с влиянием этих аллелей на фенотип.

Возьмём упрощённую модель: популяция диплоидных организмов ограничена в размере, аллели одного гена наследуются независимо от других генов, вероятность наследования одной из двух аллелей родителя - 50%, поколения
в популяции не перекрываются и численность популяции не меняется, а пул генов следующего поколения получается случайной выборкой из пула генов предыдущего поколения (это в основном совпадает с моделью
Райта-Фишера, см. https://en.wikipedia.org/wiki/Genetic_drift).

Тогда для одного конкретного гена есть (изначально) 2 аллели, с разной частотой встречаемости в исходной популяции и есть случайный процесс изменения этих частот (поскольку наследование конкретной аллели -
полностью случайный процесс). Для каждой конкретной аллели процесс может сойтись в одной из двух крайностей: полное отсутствие аллели в популяции (исчезновение) или тотальное присутствие (фиксация - частота
встречаемости аллели в популяции становится равной 100%).

## Постановка задачи

Представим популяцию диплоидных гуманоидов не имеющую социально-культурных норм и правил относительно размножения, размножающихся строго раз в году и имеющих один ген, отвечающий за цвет глаз: за голубой цвет
отвечает аллель A с исходной частотой p, а за карий цвет отвечает доминантная аллель B с исходной частотой q.

В популяции N особей и это число не меняется при смене поколений.

Модель наследования признака - простая менделевская, т.е. передача аллели потомку равновероятное, а проявление фенотипа доминантно-рецессивное, только AA особи будут иметь голубой цвет глаз.

Путем моделирования оцените, какова вероятность исчезновения или фиксации голубого цвета глаз в данной популяции. Для упрощения пусть поколения сменяются до тех пор, пока не будет достигнута фиксация аллелей.

Обратите внимание, что частота конкретной аллели и частота признака - разные вещи. У каждой особи по 2 аллели рассматриваемого гена, соответственно, если всего в популяции X копий аллели A, то её частота в данной
популяции - X/(2*N).

## Задача

Имея заготовку программы, напишите функцию, возвращающую пару чисел - вероятность исчезновения и вероятность фиксации рецессивного признака, принимающую аргументы:
- операцию генерации случайного равномерно распределённого числа от 0 до 1
- количество итераций моделирования
- размер популяции N
- начальная частота встречаемости рецессивной аллели A p
- начальная частота встречаемости доминантной аллели B q
