
set(EXAMPLES
    accessing_state
    adding_memory
    benchmark
    custom_state
    hello
    input_and_output)

foreach(example ${EXAMPLES})
    add_executable(${example} EXCLUDE_FROM_ALL "${example}.cpp")
endforeach()

add_custom_target(examples DEPENDS ${EXAMPLES})

set_target_properties(benchmark PROPERTIES COMPILE_FLAGS "-O3")
