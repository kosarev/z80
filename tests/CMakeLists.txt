
set(TESTS
    dummy_state)

foreach(test ${TESTS})
    add_executable(${test} "${test}.cpp")
    add_test(${test} ${test})
endforeach()
