cmake_minimum_required(VERSION 3.13.4)
project(Kaleidoscope)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(LLVM REQUIRED CONFIG)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_executable(kaleidoscope compiler.cpp)

target_link_libraries(kaleidoscope LLVM-21)