
add_executable(hello EXCLUDE_FROM_ALL hello.cpp)

add_custom_target(examples
  DEPENDS hello)
