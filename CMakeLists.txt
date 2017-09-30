cmake_minimum_required(VERSION 3.5)

include(CheckCXXCompilerFlag)

function(append_if condition value)
  if (${condition})
    foreach(variable ${ARGN})
      set(${variable} "${${variable}} ${value}" PARENT_SCOPE)
    endforeach(variable)
  endif()
endfunction()

project(z80)

enable_testing()

check_cxx_compiler_flag("-fdiagnostics-color" DIAGNOSTICS_COLOR_FLAG)
check_cxx_compiler_flag("-std=c++11" STD_CXX11_FLAG)

set(CXX_FLAGS)
append_if(DIAGNOSTICS_COLOR_FLAG "-fdiagnostics-color" CMAKE_CXX_FLAGS)
append_if(STD_CXX11_FLAG "-std=c++11" CMAKE_CXX_FLAGS)

add_library(z80 STATIC z80.cpp)

add_executable(tester tester.cpp)

add_test(opcodes_decoding tester)
