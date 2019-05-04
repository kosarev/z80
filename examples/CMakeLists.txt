
set(EXAMPLES
    accessing_state
    adding_memory
    hello
    input_and_output)

foreach(example ${EXAMPLES})
    add_executable(${example} EXCLUDE_FROM_ALL "${example}.cpp")
endforeach()

add_custom_target(examples DEPENDS ${EXAMPLES})
