
add_executable(tester tester.cpp)
add_test(i8080_tests tester i8080 "${CMAKE_CURRENT_SOURCE_DIR}/tests_i8080")
add_test(z80_tests tester z80 "${CMAKE_CURRENT_SOURCE_DIR}/tests_z80")

set(TESTS
    breakpoints
    dummy_state
    interrupts
    reset
    retry_input
    root
    timing
    )

foreach(test ${TESTS})
    add_executable(${test} "${test}.cpp")
    add_test(${test} ${test})
endforeach()
