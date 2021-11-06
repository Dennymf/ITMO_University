#include <iostream>
#include <fstream>
#include <vector>
#include <cstring>

std::vector<std::string> parse_str(std::istream &file)
{
    std::vector<std::string> strings;
    std::string line;

    while(getline(file, line))
    {
        strings.emplace_back(line);
    }

    return strings;
}

void count(std::istream &file1, std::istream &file2, std::ostream &out, bool options1, bool options2, bool options3)
{
    std::vector<std::string> vector1 = parse_str(file1);
    std::vector<std::string> vector2 = parse_str(file2);

    auto iterator1 = vector1.begin();
    auto iterator2 = vector2.begin();

    while (iterator1 != vector1.end() || iterator2 != vector2.end())
    {
        if (iterator2 == vector2.end() || (iterator1 != vector1.end() && *iterator1 < *iterator2))
        {
            if (options1)
            {
                out << *iterator1 << '\n';
            }
            iterator1++;
        }
        else if (iterator1 == vector1.end() || (iterator2 != vector2.end() && *iterator2 < *iterator1))
        {
            if (options2)
            {
                if(options1)
                {
                    out << '\t';
                }
                out << *iterator2 << '\n';
            }
            iterator2++;
        }
        else
        {
            if(options3)
            {
                if(options1)
                {
                    out << '\t';
                }
                if(options2)
                {
                    out << '\t';
                }
                out << *iterator1 << '\n';
            }
            iterator1++;
            iterator2++;
        }
    }
}

int main(int arg_count, char **arg_value)
{
    std::fstream file1;
    std::fstream file2;

    bool options1, options2, options3;
    options1 = options2 = options3 = true;

    if (arg_count == 4)
    {
        for(size_t i = 1; i < std::strlen(arg_value[1]); i++)
        {
            if (arg_value[1][i] == '1')
            {
                options1 = false;
            }
            else if (arg_value[1][i] == '2')
            {
                options2 = false;
            }
            else
            {
                options3 = false;
            }
        }
    }

    if (std::strcmp(arg_value[arg_count - 2], "-") != 0)
    {
        file1.open(arg_value[arg_count - 2]);
    }
    if (std::strcmp(arg_value[arg_count - 1], "-") != 0)
    {
        file2.open(arg_value[arg_count - 1]);
    }

    count(file1.is_open() ? file1 : std::cin,file2.is_open() ? file2 : std::cin,std::cout, options1, options2, options3);

    if (file1.is_open())
    {
        file1.close();
    }
    if (file2.is_open())
    {
        file2.close();
    }
}