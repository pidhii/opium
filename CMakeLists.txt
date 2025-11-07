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

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wall -Wextra -Werror -Wno-error=unused-parameter -rdynamic")

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

# Enable/disable benchmarks
option (OPIUM_FUNCTION_BENCHMARKS "Enable function benchmarks" OFF)
option (OPIUM_BLOCK_BENCHMARKS "Enable code block benchmarks" OFF)
if (OPIUM_FUNCTION_BENCHMARKS)
  add_compile_definitions (OPI_FUNCTION_BENCHMARKS_ENABLED)
endif (OPIUM_FUNCTION_BENCHMARKS)
if (OPIUM_BLOCK_BENCHMARKS)
  add_compile_definitions (OPI_BLOCK_BENCHMARKS_ENABLED)
endif (OPIUM_BLOCK_BENCHMARKS)

# Create an object library for all source files
file (GLOB src_files src/*.cpp)
add_library(opium_objects OBJECT ${src_files})
target_include_directories(opium_objects PUBLIC ${PROJECT_SOURCE_DIR}/include ${READLINE_INCLUDE_DIRS})
target_compile_options(opium_objects PUBLIC ${READLINE_CFLAGS_OTHER})
set_target_properties(opium_objects PROPERTIES POSITION_INDEPENDENT_CODE ON)

# Shared library
add_library (opium_so SHARED $<TARGET_OBJECTS:opium_objects>)
target_link_libraries (opium_so PUBLIC -lgc ${Boost_LIBRARIES} ${READLINE_LIBRARIES})
set_target_properties (opium_so PROPERTIES OUTPUT_NAME opium)

# Static library
add_library (opium_a STATIC $<TARGET_OBJECTS:opium_objects>)
target_link_libraries (opium_a PUBLIC -lgc ${Boost_LIBRARIES} ${READLINE_LIBRARIES})
set_target_properties (opium_a PROPERTIES OUTPUT_NAME opium)

install (DIRECTORY ${PROJECT_SOURCE_DIR}/include/opium DESTINATION include)
install (TARGETS opium_so opium_a DESTINATION lib)

# Main executable
file (GLOB opium_exec_src apps/opium/*.cpp)
add_executable (opium ${opium_exec_src} $<TARGET_OBJECTS:opium_objects>)
target_include_directories (opium PUBLIC ${PROJECT_SOURCE_DIR}/include)
target_link_libraries (opium PUBLIC -lgc ${Boost_LIBRARIES} ${READLINE_LIBRARIES})
target_include_directories(opium PUBLIC ${READLINE_INCLUDE_DIRS})
target_compile_options(opium PUBLIC ${READLINE_CFLAGS_OTHER})

# Preprocessor executable
file (GLOB opium_pp_exec_src apps/opium-pp/*.cpp)
add_executable (opium-pp ${opium_pp_exec_src} $<TARGET_OBJECTS:opium_objects>)
target_include_directories (opium-pp PUBLIC ${PROJECT_SOURCE_DIR}/include)
target_link_libraries (opium-pp PUBLIC -lgc ${Boost_LIBRARIES})
target_include_directories(opium-pp PUBLIC ${READLINE_INCLUDE_DIRS})
target_compile_options(opium-pp PUBLIC ${READLINE_CFLAGS_OTHER})

install (TARGETS opium opium-pp DESTINATION bin)

set (ECO_BUILD_TESTS OFF)
set (ECO_WITH_VALGRIND OFF)
add_subdirectory(eco)

# Add eco include directories to opium targets
target_include_directories(opium_objects PUBLIC ${PROJECT_SOURCE_DIR}/eco/include)

# Link eco libraries to opium targets
target_link_libraries(opium_so PUBLIC eco_so)
target_link_libraries(opium_a PUBLIC eco_a)

# Link eco to executables
target_link_libraries(opium PUBLIC eco_a)
target_link_libraries(opium-pp PUBLIC eco_a)

configure_file(${PROJECT_SOURCE_DIR}/opium.pc ${CMAKE_BINARY_DIR}/opium.pc @ONLY)
install(FILES ${CMAKE_BINARY_DIR}/opium.pc DESTINATION lib/pkgconfig)

option (OPIUM_SCRIPT "Build opium scripting language" ON)
if (OPIUM_SCRIPT)
  find_package (BISON REQUIRED)
  find_package (FLEX REQUIRED)

  set (osl_prefix ${PROJECT_SOURCE_DIR}/osl)

  # Setup Bison and Felx targets
  BISON_TARGET (osl_parser ${osl_prefix}/syntax/parser.ypp ${CMAKE_BINARY_DIR}/osl_parser.cpp)
  FLEX_TARGET (osl_scanner ${osl_prefix}/syntax/scanner.l ${CMAKE_BINARY_DIR}/osl_scanner.cpp)
  ADD_FLEX_BISON_DEPENDENCY (osl_scanner osl_parser)

  # Setup syntax_definitions.h with contents of syntax-definitions.opi
  file(READ "${osl_prefix}/syntax-definitions.opi" osl_syntax_definitions)
  configure_file("${osl_prefix}/syntax_definitions.h.in"
                 "${CMAKE_BINARY_DIR}/syntax_definitions.h" @ONLY)

  # Opium plugins
  file (GLOB osl_opium_plugins ${osl_prefix}/plugins/*.cpp)
  foreach (plugin_src ${osl_opium_plugins})
    get_filename_component (plugin_name ${plugin_src} NAME_WE)
    add_library (${plugin_name} SHARED ${plugin_src})
    target_include_directories (${plugin_name} PUBLIC ${PROJECT_SOURCE_DIR}/include)
    set_target_properties (${plugin_name}
      PROPERTIES
        POSITION_INDEPENDENT_CODE ON
        PREFIX ""
        SUFFIX ".plugin")
    install (TARGETS ${plugin_name} DESTINATION lib)
  endforeach ()

  # OSL executable
  file (GLOB osl_src_files ${osl_prefix}/*.cpp)
  set(osl_syntax_src ${BISON_osl_parser_OUTPUTS} ${FLEX_osl_scanner_OUTPUTS})
  add_executable (osl apps/osl/main.cpp ${osl_src_files} ${osl_syntax_src})
  target_compile_options (osl PRIVATE -Wno-error=unused-function)
  target_include_directories(osl
    PUBLIC
      ${osl_prefix}
      ${PROJECT_BINARY_DIR}
      ${PROJECT_SOURCE_DIR}/include
      ${PROJECT_SOURCE_DIR}/eco/include)
  target_link_libraries (osl opium_a eco_a)
  install (TARGETS osl DESTINATION bin)

  # Builtin OSL libraries
  file(GLOB osl_libs osl/lib/*)
  install(FILES ${osl_libs} DESTINATION lib)

  # OSL compiler and interpreter
  if ("${CMAKE_INSTALL_PREFIX}" EQUAL "")
    set(OSL_LIB_DIR ${PROJECT_SOURCE_DIR}/osl/lib)
  else ()
    set(OSL_LIB_DIR ${CMAKE_INSTALL_PREFIX}/lib)
  endif ()
  add_custom_command(
    OUTPUT ${CMAKE_BINARY_DIR}/opium-osl
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    COMMAND echo "'default_oslpath=\"${OSL_LIB_DIR}\"'" > ${CMAKE_BINARY_DIR}/osl-config.osl
    COMMAND ${CMAKE_BINARY_DIR}/osl -o ${CMAKE_BINARY_DIR}/opium-osl.scm
                                    --oslpath ${PROJECT_SOURCE_DIR}/osl/lib
                                    --oslpath ${CMAKE_BINARY_DIR}
                                    ${PROJECT_SOURCE_DIR}/opium-osl.osl
    COMMAND chicken-csc -o ${CMAKE_BINARY_DIR}/opium-osl ${CMAKE_BINARY_DIR}/opium-osl.scm
    BYPRODUCTS ${CMAKE_BINARY_DIR}/opium-osl.scm ${CMAKE_BINARY_DIR}/osl-config.osl
    DEPENDS osl ${PROJECT_SOURCE_DIR}/opium-osl.osl)
  add_custom_target(opium-osl-executable ALL DEPENDS ${CMAKE_BINARY_DIR}/opium-osl)
  install(PROGRAMS ${CMAKE_BINARY_DIR}/opium-osl DESTINATION bin)

endif (OPIUM_SCRIPT)

option (OPIUM_UNIT_TESTS "Enable unit tests for opium components" ON)
if (OPIUM_UNIT_TESTS)
  # Disable installtion of GTest
  set (INSTALL_GTEST OFF)

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
  target_include_directories (test_predicate_runtime PUBLIC ${PROJECT_SOURCE_DIR}/include)
  target_link_libraries (test_predicate_runtime PUBLIC opium_objects -lgc GTest::gtest_main eco_a)

  # LISP Parser Tests
  add_executable (test_lisp_parser tests/test_lisp_parser.cpp)
  target_include_directories (test_lisp_parser PUBLIC ${PROJECT_SOURCE_DIR}/include)
  target_link_libraries (test_lisp_parser PUBLIC opium_objects -lgc GTest::gtest_main eco_a)

  # Prolog Tests
  add_executable (test_prolog tests/test_prolog.cpp)
  target_include_directories (test_prolog PUBLIC ${PROJECT_SOURCE_DIR}/include)
  target_link_libraries (test_prolog PUBLIC opium_objects -lgc GTest::gtest_main eco_a)

  # LISP Reader Tests
  add_executable (test_lisp_reader tests/test_lisp_reader.cpp)
  target_include_directories (test_lisp_reader PUBLIC ${PROJECT_SOURCE_DIR}/include)
  target_link_libraries (test_lisp_reader PUBLIC opium_objects -lgc GTest::gtest_main eco_a)

  # Equality Tests
  add_executable (test_equality tests/test_equality.cpp)
  target_include_directories (test_equality PUBLIC ${PROJECT_SOURCE_DIR}/include)
  target_link_libraries (test_equality PUBLIC opium_objects -lgc GTest::gtest_main eco_a)

  # Match Tests
  add_executable (test_match tests/test_match.cpp)
  target_include_directories (test_match PUBLIC ${PROJECT_SOURCE_DIR}/include)
  target_link_libraries (test_match PUBLIC opium_objects -lgc GTest::gtest_main eco_a)

  # Scheme Unique Identifiers Tests
  add_executable (test_scheme_unique_identifiers tests/test_scheme_unique_identifiers.cpp)
  target_include_directories (test_scheme_unique_identifiers PUBLIC ${PROJECT_SOURCE_DIR}/include)
  target_link_libraries (test_scheme_unique_identifiers PUBLIC opium_objects -lgc GTest::gtest_main eco_a)

  # Quotation Tests
  add_executable (test_quotation tests/test_quotation.cpp)
  target_include_directories (test_quotation PUBLIC ${PROJECT_SOURCE_DIR}/include)
  target_link_libraries (test_quotation PUBLIC opium_objects -lgc GTest::gtest_main eco_a)

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
