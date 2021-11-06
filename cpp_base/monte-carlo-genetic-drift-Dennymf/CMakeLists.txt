cmake_minimum_required(VERSION 3.0)

set(PROJECT_NAME monte_carlo_genetic_drift)
project(${PROJECT_NAME})

set(CMAKE_CXX_FLAGS "-g -o -Wall -std=gnu++17 -D_WIN32_WINNT=0x0501 -Wno-deprecated-declarations")

# Inlcude directories
set(COMMON_INCLUDES ${PROJECT_SOURCE_DIR}/include)
include_directories(${COMMON_INCLUDES})

# Source files
file(GLOB SRC_FILES ${PROJECT_SOURCE_DIR}/src/*.cpp)

# Separate executable: main
list(REMOVE_ITEM SRC_FILES ${PROJECT_SOURCE_DIR}/src/main.cpp)

# Compile source files into a library
add_library(monte_carlo_genetic_drift_lib ${SRC_FILES})

# Main is separate
add_executable(monte-carlo-genetic-drift ${PROJECT_SOURCE_DIR}/src/main.cpp)

# linking Main against the library
target_link_libraries(monte-carlo-genetic-drift monte_carlo_genetic_drift_lib)

# google test is a git submodule
add_subdirectory(./googletest)

enable_testing()

# Include the gtest library
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

# Test files
file(GLOB TEST_SRC_FILES ${PROJECT_SOURCE_DIR}/test/*.cpp)

# Unit tests
add_executable(runUnitTests ${TEST_SRC_FILES})

# Standard linking to gtest stuff
target_link_libraries(runUnitTests gtest gtest_main)

# Extra linking for the project
target_link_libraries(runUnitTests monte_carlo_genetic_drift_lib)
