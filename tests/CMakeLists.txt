
set(TESTS
    i8080_dummy_state
    z80_dummy_state)

foreach(test ${TESTS})
    add_executable(${test} "${test}.cpp")
    add_test(${test} ${test})
endforeach()
