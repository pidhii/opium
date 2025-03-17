cmake_minimum_required (VERSION 3.15)
project (opium VERSION 0.1.0)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS TRUE)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wall -Wextra -Werror -Wno-error=unused-parameter")

set (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS} -O3")
set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS} -Og -ggdb")

# Enable testing
enable_testing()

# Include FetchContent for downloading dependencies
include(FetchContent)

# Download and configure Google Test
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG v1.16.0
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

# Main executable
file (GLOB src src/*.cpp)
add_executable (opium main.cpp ${src})
target_include_directories (opium PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_link_libraries(opium PUBLIC -lgc)

install (TARGETS opium DESTINATION bin)

# Tests
add_executable(test_predicate_runtime tests/test_predicate_runtime.cpp ${src})
target_include_directories(test_predicate_runtime PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_link_libraries(test_predicate_runtime PUBLIC -lgc GTest::gtest_main)

# LISP Parser Tests
add_executable(test_lisp_parser tests/test_lisp_parser.cpp ${src})
target_include_directories(test_lisp_parser PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_link_libraries(test_lisp_parser PUBLIC -lgc GTest::gtest_main)

# Register tests with CTest
include(GoogleTest)
gtest_discover_tests(test_predicate_runtime)
gtest_discover_tests(test_lisp_parser)
