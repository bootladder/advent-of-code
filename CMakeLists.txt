cmake_minimum_required(VERSION 3.15)
project(advent_of_code_2020)

set(CMAKE_CXX_STANDARD 14)

find_package(Boost 1.1)
#if(Boost_FOUND)
include_directories(${Boost_INCLUDE_DIRS})
#endif()

include_directories(help)

add_executable(advent_of_code_2020
        day1/help.cpp
        day1/day1.cpp)

add_executable(advent_of_code_2020_day1_2
        day1/help.cpp
        day1/day1-2.cpp
        )

add_executable(day2_1
        day2/day2-1.cpp
        )

add_executable(day2_2
        day2/day2-2.cpp
        )

add_executable(day3-1
        day3/day3-1.cpp
        help/filehelp.cpp
        )
add_executable(day3-2
        day3/day3-2.cpp
        help/filehelp.cpp
        )