
set(EXAMPLES
    adding_memory
    hello)

foreach(example ${EXAMPLES})
    add_executable(${example} EXCLUDE_FROM_ALL "${example}.cpp")
endforeach()

add_custom_target(examples DEPENDS ${EXAMPLES})
