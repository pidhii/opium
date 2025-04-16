# Opium - Ultimate static type system for type-annotation-free code
# Copyright (C) 2025  Ivan Pidhurskyi
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


cmake_minimum_required (VERSION 3.15)
project (opium VERSION 0.1.0)

set (CMAKE_CXX_STANDARD 20)
set (CMAKE_EXPORT_COMPILE_COMMANDS TRUE)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wall -Wextra -Werror -Wno-error=unused-parameter")

set (CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS} -O3 -DOPIUM_RELEASE_BUILD")
set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS} -O0 -ggdb")

# Enable testing
enable_testing ()

# Include FetchContent for downloading dependencies
include (FetchContent)

# Find Boost
cmake_policy (SET CMP0167 NEW)
find_package (Boost REQUIRED COMPONENTS program_options)

# Find Readline
find_package (PkgConfig REQUIRED)
pkg_check_modules (READLINE REQUIRED readline)

# Find Doxygen
find_package(Doxygen)
if (DOXYGEN_FOUND)
  add_custom_target (docs
    COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Generating API documentation with Doxygen"
    VERBATIM)
endif ()

# Create an object library for all source files
file (GLOB src_files src/*.cpp)
add_library(opium_objects OBJECT ${src_files})
target_include_directories(opium_objects PUBLIC ${CMAKE_SOURCE_DIR}/include ${READLINE_INCLUDE_DIRS})
target_compile_options(opium_objects PUBLIC ${READLINE_CFLAGS_OTHER})

# Main executable
file (GLOB opium_exec_src apps/opium/*.cpp)
add_executable (opium ${opium_exec_src} $<TARGET_OBJECTS:opium_objects>)
target_include_directories (opium PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_link_libraries (opium PUBLIC -lgc ${Boost_LIBRARIES} ${READLINE_LIBRARIES})
target_include_directories(opium PUBLIC ${READLINE_INCLUDE_DIRS})
target_compile_options(opium PUBLIC ${READLINE_CFLAGS_OTHER})

install (TARGETS opium DESTINATION bin)

option (OPIUM_UNIT_TESTS "Enable unit tests for opium components" ON)
if (OPIUM_UNIT_TESTS)
  # Download and configure Google Test
  FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG v1.16.0
  )
  # For Windows: Prevent overriding the parent project's compiler/linker settings
  set (gtest_force_shared_crt ON CACHE BOOL "" FORCE)
  FetchContent_MakeAvailable(googletest)

  # Tests
  add_executable (test_predicate_runtime tests/test_predicate_runtime.cpp)
  target_include_directories (test_predicate_runtime PUBLIC ${CMAKE_SOURCE_DIR}/include)
  target_link_libraries (test_predicate_runtime PUBLIC opium_objects -lgc GTest::gtest_main)

  # LISP Parser Tests
  add_executable (test_lisp_parser tests/test_lisp_parser.cpp)
  target_include_directories (test_lisp_parser PUBLIC ${CMAKE_SOURCE_DIR}/include)
  target_link_libraries (test_lisp_parser PUBLIC opium_objects -lgc GTest::gtest_main)

  # Prolog Tests
  add_executable (test_prolog tests/test_prolog.cpp)
  target_include_directories (test_prolog PUBLIC ${CMAKE_SOURCE_DIR}/include)
  target_link_libraries (test_prolog PUBLIC opium_objects -lgc GTest::gtest_main)

  # LISP Reader Tests
  add_executable (test_lisp_reader tests/test_lisp_reader.cpp)
  target_include_directories (test_lisp_reader PUBLIC ${CMAKE_SOURCE_DIR}/include)
  target_link_libraries (test_lisp_reader PUBLIC opium_objects -lgc GTest::gtest_main)

  # Equality Tests
  add_executable (test_equality tests/test_equality.cpp)
  target_include_directories (test_equality PUBLIC ${CMAKE_SOURCE_DIR}/include)
  target_link_libraries (test_equality PUBLIC opium_objects -lgc GTest::gtest_main)

  # Match Tests
  add_executable (test_match tests/test_match.cpp)
  target_include_directories (test_match PUBLIC ${CMAKE_SOURCE_DIR}/include)
  target_link_libraries (test_match PUBLIC opium_objects -lgc GTest::gtest_main)

  # Scheme Unique Identifiers Tests
  add_executable (test_scheme_unique_identifiers tests/test_scheme_unique_identifiers.cpp)
  target_include_directories (test_scheme_unique_identifiers PUBLIC ${CMAKE_SOURCE_DIR}/include)
  target_link_libraries (test_scheme_unique_identifiers PUBLIC opium_objects -lgc GTest::gtest_main)

  # Quotation Tests
  add_executable (test_quotation tests/test_quotation.cpp)
  target_include_directories (test_quotation PUBLIC ${CMAKE_SOURCE_DIR}/include)
  target_link_libraries (test_quotation PUBLIC opium_objects -lgc GTest::gtest_main)

  # Register tests with CTest
  include (GoogleTest)
  gtest_discover_tests (test_predicate_runtime)
  gtest_discover_tests (test_lisp_parser)
  gtest_discover_tests (test_prolog)
  gtest_discover_tests (test_lisp_reader)
  gtest_discover_tests (test_equality)
  gtest_discover_tests (test_match)
  gtest_discover_tests (test_scheme_unique_identifiers)
  gtest_discover_tests (test_quotation)
endif (OPIUM_UNIT_TESTS)
