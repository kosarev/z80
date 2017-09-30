cmake_minimum_required(VERSION 3.5)

project(z80)

enable_testing()

add_executable(tester tester.cpp)

add_test(opcodes_decoding tester)
