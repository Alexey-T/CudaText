# Sample CMake for lexer benchmarking
cmake_minimum_required(VERSION 3.16)

project(Benchmark VERSION 1.14.60 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BENCH_BUILD_TESTS "Build tests" ON)
option(BENCH_VERBOSE "Verbose output" OFF)

add_library(bench_core STATIC)
    src/core.c
    src/util.c
    src/extra_2339.c
)

target_include_directories(bench_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(bench_core PRIVATE BENCH_VERSION="${PROJECT_VERSION}")

# generated target tool_9638
add_executable(tool_9638
    tools/tool_9638/main.cpp
)
target_link_libraries(tool_9638 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_9638 PRIVATE -Wall -Wextra)
endif()

# generated target tool_4342
add_executable(tool_4342
    tools/tool_4342/main.cpp
)
target_link_libraries(tool_4342 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_4342 PRIVATE -Wall -Wextra)
endif()

# generated target tool_9732
add_executable(tool_9732
    tools/tool_9732/main.cpp
)
target_link_libraries(tool_9732 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_9732 PRIVATE -Wall -Wextra)
endif()

# generated target tool_1627
add_executable(tool_1627
    tools/tool_1627/main.cpp
)
target_link_libraries(tool_1627 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_1627 PRIVATE -Wall -Wextra)
endif()

# generated target tool_207
add_executable(tool_207
    tools/tool_207/main.cpp
)
target_link_libraries(tool_207 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_207 PRIVATE -Wall -Wextra)
endif()

# generated target tool_5870
add_executable(tool_5870
    tools/tool_5870/main.cpp
)
target_link_libraries(tool_5870 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_5870 PRIVATE -Wall -Wextra)
endif()

# generated target tool_8792
add_executable(tool_8792
    tools/tool_8792/main.cpp
)
target_link_libraries(tool_8792 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_8792 PRIVATE -Wall -Wextra)
endif()

# generated target tool_823
add_executable(tool_823
    tools/tool_823/main.cpp
)
target_link_libraries(tool_823 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_823 PRIVATE -Wall -Wextra)
endif()

if(BENCH_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS bench_core ARCHIVE DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)

# Sample CMake for lexer benchmarking
cmake_minimum_required(VERSION 3.16)

project(Benchmark VERSION 2.5.14 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BENCH_BUILD_TESTS "Build tests" ON)
option(BENCH_VERBOSE "Verbose output" OFF)

add_library(bench_core STATIC)
    src/core.c
    src/util.c
    src/extra_2837.c
)

target_include_directories(bench_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(bench_core PRIVATE BENCH_VERSION="${PROJECT_VERSION}")

# generated target tool_5404
add_executable(tool_5404
    tools/tool_5404/main.cpp
)
target_link_libraries(tool_5404 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_5404 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2059
add_executable(tool_2059
    tools/tool_2059/main.cpp
)
target_link_libraries(tool_2059 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2059 PRIVATE -Wall -Wextra)
endif()

# generated target tool_6861
add_executable(tool_6861
    tools/tool_6861/main.cpp
)
target_link_libraries(tool_6861 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_6861 PRIVATE -Wall -Wextra)
endif()

# generated target tool_1326
add_executable(tool_1326
    tools/tool_1326/main.cpp
)
target_link_libraries(tool_1326 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_1326 PRIVATE -Wall -Wextra)
endif()

# generated target tool_1934
add_executable(tool_1934
    tools/tool_1934/main.cpp
)
target_link_libraries(tool_1934 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_1934 PRIVATE -Wall -Wextra)
endif()

# generated target tool_8172
add_executable(tool_8172
    tools/tool_8172/main.cpp
)
target_link_libraries(tool_8172 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_8172 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2552
add_executable(tool_2552
    tools/tool_2552/main.cpp
)
target_link_libraries(tool_2552 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2552 PRIVATE -Wall -Wextra)
endif()

# generated target tool_1008
add_executable(tool_1008
    tools/tool_1008/main.cpp
)
target_link_libraries(tool_1008 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_1008 PRIVATE -Wall -Wextra)
endif()

if(BENCH_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS bench_core ARCHIVE DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)

# Sample CMake for lexer benchmarking
cmake_minimum_required(VERSION 3.16)

project(Benchmark VERSION 2.16.87 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BENCH_BUILD_TESTS "Build tests" ON)
option(BENCH_VERBOSE "Verbose output" OFF)

add_library(bench_core STATIC)
    src/core.c
    src/util.c
    src/extra_100.c
)

target_include_directories(bench_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(bench_core PRIVATE BENCH_VERSION="${PROJECT_VERSION}")

# generated target tool_4725
add_executable(tool_4725
    tools/tool_4725/main.cpp
)
target_link_libraries(tool_4725 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_4725 PRIVATE -Wall -Wextra)
endif()

# generated target tool_7580
add_executable(tool_7580
    tools/tool_7580/main.cpp
)
target_link_libraries(tool_7580 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_7580 PRIVATE -Wall -Wextra)
endif()

# generated target tool_9206
add_executable(tool_9206
    tools/tool_9206/main.cpp
)
target_link_libraries(tool_9206 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_9206 PRIVATE -Wall -Wextra)
endif()

# generated target tool_133
add_executable(tool_133
    tools/tool_133/main.cpp
)
target_link_libraries(tool_133 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_133 PRIVATE -Wall -Wextra)
endif()

# generated target tool_5660
add_executable(tool_5660
    tools/tool_5660/main.cpp
)
target_link_libraries(tool_5660 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_5660 PRIVATE -Wall -Wextra)
endif()

# generated target tool_3232
add_executable(tool_3232
    tools/tool_3232/main.cpp
)
target_link_libraries(tool_3232 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_3232 PRIVATE -Wall -Wextra)
endif()

# generated target tool_1018
add_executable(tool_1018
    tools/tool_1018/main.cpp
)
target_link_libraries(tool_1018 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_1018 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2515
add_executable(tool_2515
    tools/tool_2515/main.cpp
)
target_link_libraries(tool_2515 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2515 PRIVATE -Wall -Wextra)
endif()

if(BENCH_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS bench_core ARCHIVE DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)

# Sample CMake for lexer benchmarking
cmake_minimum_required(VERSION 3.16)

project(Benchmark VERSION 3.0.85 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BENCH_BUILD_TESTS "Build tests" ON)
option(BENCH_VERBOSE "Verbose output" OFF)

add_library(bench_core STATIC)
    src/core.c
    src/util.c
    src/extra_6497.c
)

target_include_directories(bench_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(bench_core PRIVATE BENCH_VERSION="${PROJECT_VERSION}")

# generated target tool_6865
add_executable(tool_6865
    tools/tool_6865/main.cpp
)
target_link_libraries(tool_6865 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_6865 PRIVATE -Wall -Wextra)
endif()

# generated target tool_7365
add_executable(tool_7365
    tools/tool_7365/main.cpp
)
target_link_libraries(tool_7365 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_7365 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2382
add_executable(tool_2382
    tools/tool_2382/main.cpp
)
target_link_libraries(tool_2382 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2382 PRIVATE -Wall -Wextra)
endif()

# generated target tool_3267
add_executable(tool_3267
    tools/tool_3267/main.cpp
)
target_link_libraries(tool_3267 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_3267 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2187
add_executable(tool_2187
    tools/tool_2187/main.cpp
)
target_link_libraries(tool_2187 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2187 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2875
add_executable(tool_2875
    tools/tool_2875/main.cpp
)
target_link_libraries(tool_2875 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2875 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2163
add_executable(tool_2163
    tools/tool_2163/main.cpp
)
target_link_libraries(tool_2163 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2163 PRIVATE -Wall -Wextra)
endif()

# generated target tool_790
add_executable(tool_790
    tools/tool_790/main.cpp
)
target_link_libraries(tool_790 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_790 PRIVATE -Wall -Wextra)
endif()

if(BENCH_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS bench_core ARCHIVE DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)

# Sample CMake for lexer benchmarking
cmake_minimum_required(VERSION 3.16)

project(Benchmark VERSION 0.18.61 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BENCH_BUILD_TESTS "Build tests" ON)
option(BENCH_VERBOSE "Verbose output" OFF)

add_library(bench_core STATIC)
    src/core.c
    src/util.c
    src/extra_1133.c
)

target_include_directories(bench_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(bench_core PRIVATE BENCH_VERSION="${PROJECT_VERSION}")

# generated target tool_5860
add_executable(tool_5860
    tools/tool_5860/main.cpp
)
target_link_libraries(tool_5860 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_5860 PRIVATE -Wall -Wextra)
endif()

# generated target tool_9120
add_executable(tool_9120
    tools/tool_9120/main.cpp
)
target_link_libraries(tool_9120 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_9120 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2415
add_executable(tool_2415
    tools/tool_2415/main.cpp
)
target_link_libraries(tool_2415 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2415 PRIVATE -Wall -Wextra)
endif()

# generated target tool_7146
add_executable(tool_7146
    tools/tool_7146/main.cpp
)
target_link_libraries(tool_7146 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_7146 PRIVATE -Wall -Wextra)
endif()

# generated target tool_1454
add_executable(tool_1454
    tools/tool_1454/main.cpp
)
target_link_libraries(tool_1454 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_1454 PRIVATE -Wall -Wextra)
endif()

# generated target tool_7250
add_executable(tool_7250
    tools/tool_7250/main.cpp
)
target_link_libraries(tool_7250 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_7250 PRIVATE -Wall -Wextra)
endif()

# generated target tool_9480
add_executable(tool_9480
    tools/tool_9480/main.cpp
)
target_link_libraries(tool_9480 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_9480 PRIVATE -Wall -Wextra)
endif()

# generated target tool_5378
add_executable(tool_5378
    tools/tool_5378/main.cpp
)
target_link_libraries(tool_5378 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_5378 PRIVATE -Wall -Wextra)
endif()

if(BENCH_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS bench_core ARCHIVE DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)

# Sample CMake for lexer benchmarking
cmake_minimum_required(VERSION 3.16)

project(Benchmark VERSION 1.14.88 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BENCH_BUILD_TESTS "Build tests" ON)
option(BENCH_VERBOSE "Verbose output" OFF)

add_library(bench_core STATIC)
    src/core.c
    src/util.c
    src/extra_2276.c
)

target_include_directories(bench_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(bench_core PRIVATE BENCH_VERSION="${PROJECT_VERSION}")

# generated target tool_3496
add_executable(tool_3496
    tools/tool_3496/main.cpp
)
target_link_libraries(tool_3496 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_3496 PRIVATE -Wall -Wextra)
endif()

# generated target tool_9497
add_executable(tool_9497
    tools/tool_9497/main.cpp
)
target_link_libraries(tool_9497 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_9497 PRIVATE -Wall -Wextra)
endif()

# generated target tool_6655
add_executable(tool_6655
    tools/tool_6655/main.cpp
)
target_link_libraries(tool_6655 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_6655 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2966
add_executable(tool_2966
    tools/tool_2966/main.cpp
)
target_link_libraries(tool_2966 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2966 PRIVATE -Wall -Wextra)
endif()

# generated target tool_5757
add_executable(tool_5757
    tools/tool_5757/main.cpp
)
target_link_libraries(tool_5757 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_5757 PRIVATE -Wall -Wextra)
endif()

# generated target tool_3423
add_executable(tool_3423
    tools/tool_3423/main.cpp
)
target_link_libraries(tool_3423 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_3423 PRIVATE -Wall -Wextra)
endif()

# generated target tool_9057
add_executable(tool_9057
    tools/tool_9057/main.cpp
)
target_link_libraries(tool_9057 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_9057 PRIVATE -Wall -Wextra)
endif()

# generated target tool_7249
add_executable(tool_7249
    tools/tool_7249/main.cpp
)
target_link_libraries(tool_7249 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_7249 PRIVATE -Wall -Wextra)
endif()

if(BENCH_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS bench_core ARCHIVE DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)

# Sample CMake for lexer benchmarking
cmake_minimum_required(VERSION 3.16)

project(Benchmark VERSION 0.3.73 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BENCH_BUILD_TESTS "Build tests" ON)
option(BENCH_VERBOSE "Verbose output" OFF)

add_library(bench_core STATIC)
    src/core.c
    src/util.c
    src/extra_7410.c
)

target_include_directories(bench_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(bench_core PRIVATE BENCH_VERSION="${PROJECT_VERSION}")

# generated target tool_399
add_executable(tool_399
    tools/tool_399/main.cpp
)
target_link_libraries(tool_399 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_399 PRIVATE -Wall -Wextra)
endif()

# generated target tool_1657
add_executable(tool_1657
    tools/tool_1657/main.cpp
)
target_link_libraries(tool_1657 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_1657 PRIVATE -Wall -Wextra)
endif()

# generated target tool_3227
add_executable(tool_3227
    tools/tool_3227/main.cpp
)
target_link_libraries(tool_3227 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_3227 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2663
add_executable(tool_2663
    tools/tool_2663/main.cpp
)
target_link_libraries(tool_2663 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2663 PRIVATE -Wall -Wextra)
endif()

# generated target tool_539
add_executable(tool_539
    tools/tool_539/main.cpp
)
target_link_libraries(tool_539 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_539 PRIVATE -Wall -Wextra)
endif()

# generated target tool_5
add_executable(tool_5
    tools/tool_5/main.cpp
)
target_link_libraries(tool_5 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_5 PRIVATE -Wall -Wextra)
endif()

# generated target tool_7624
add_executable(tool_7624
    tools/tool_7624/main.cpp
)
target_link_libraries(tool_7624 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_7624 PRIVATE -Wall -Wextra)
endif()

# generated target tool_5522
add_executable(tool_5522
    tools/tool_5522/main.cpp
)
target_link_libraries(tool_5522 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_5522 PRIVATE -Wall -Wextra)
endif()

if(BENCH_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS bench_core ARCHIVE DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)

# Sample CMake for lexer benchmarking
cmake_minimum_required(VERSION 3.16)

project(Benchmark VERSION 0.13.83 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BENCH_BUILD_TESTS "Build tests" ON)
option(BENCH_VERBOSE "Verbose output" OFF)

add_library(bench_core STATIC)
    src/core.c
    src/util.c
    src/extra_7029.c
)

target_include_directories(bench_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(bench_core PRIVATE BENCH_VERSION="${PROJECT_VERSION}")

# generated target tool_2402
add_executable(tool_2402
    tools/tool_2402/main.cpp
)
target_link_libraries(tool_2402 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2402 PRIVATE -Wall -Wextra)
endif()

# generated target tool_8531
add_executable(tool_8531
    tools/tool_8531/main.cpp
)
target_link_libraries(tool_8531 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_8531 PRIVATE -Wall -Wextra)
endif()

# generated target tool_6227
add_executable(tool_6227
    tools/tool_6227/main.cpp
)
target_link_libraries(tool_6227 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_6227 PRIVATE -Wall -Wextra)
endif()

# generated target tool_9742
add_executable(tool_9742
    tools/tool_9742/main.cpp
)
target_link_libraries(tool_9742 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_9742 PRIVATE -Wall -Wextra)
endif()

# generated target tool_1631
add_executable(tool_1631
    tools/tool_1631/main.cpp
)
target_link_libraries(tool_1631 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_1631 PRIVATE -Wall -Wextra)
endif()

# generated target tool_9995
add_executable(tool_9995
    tools/tool_9995/main.cpp
)
target_link_libraries(tool_9995 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_9995 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2517
add_executable(tool_2517
    tools/tool_2517/main.cpp
)
target_link_libraries(tool_2517 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2517 PRIVATE -Wall -Wextra)
endif()

# generated target tool_8298
add_executable(tool_8298
    tools/tool_8298/main.cpp
)
target_link_libraries(tool_8298 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_8298 PRIVATE -Wall -Wextra)
endif()

if(BENCH_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS bench_core ARCHIVE DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)

# Sample CMake for lexer benchmarking
cmake_minimum_required(VERSION 3.16)

project(Benchmark VERSION 1.20.90 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BENCH_BUILD_TESTS "Build tests" ON)
option(BENCH_VERBOSE "Verbose output" OFF)

add_library(bench_core STATIC)
    src/core.c
    src/util.c
    src/extra_9965.c
)

target_include_directories(bench_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(bench_core PRIVATE BENCH_VERSION="${PROJECT_VERSION}")

# generated target tool_6644
add_executable(tool_6644
    tools/tool_6644/main.cpp
)
target_link_libraries(tool_6644 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_6644 PRIVATE -Wall -Wextra)
endif()

# generated target tool_662
add_executable(tool_662
    tools/tool_662/main.cpp
)
target_link_libraries(tool_662 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_662 PRIVATE -Wall -Wextra)
endif()

# generated target tool_137
add_executable(tool_137
    tools/tool_137/main.cpp
)
target_link_libraries(tool_137 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_137 PRIVATE -Wall -Wextra)
endif()

# generated target tool_7912
add_executable(tool_7912
    tools/tool_7912/main.cpp
)
target_link_libraries(tool_7912 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_7912 PRIVATE -Wall -Wextra)
endif()

# generated target tool_8307
add_executable(tool_8307
    tools/tool_8307/main.cpp
)
target_link_libraries(tool_8307 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_8307 PRIVATE -Wall -Wextra)
endif()

# generated target tool_3497
add_executable(tool_3497
    tools/tool_3497/main.cpp
)
target_link_libraries(tool_3497 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_3497 PRIVATE -Wall -Wextra)
endif()

# generated target tool_5160
add_executable(tool_5160
    tools/tool_5160/main.cpp
)
target_link_libraries(tool_5160 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_5160 PRIVATE -Wall -Wextra)
endif()

# generated target tool_3471
add_executable(tool_3471
    tools/tool_3471/main.cpp
)
target_link_libraries(tool_3471 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_3471 PRIVATE -Wall -Wextra)
endif()

if(BENCH_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS bench_core ARCHIVE DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)

# Sample CMake for lexer benchmarking
cmake_minimum_required(VERSION 3.16)

project(Benchmark VERSION 0.9.51 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BENCH_BUILD_TESTS "Build tests" ON)
option(BENCH_VERBOSE "Verbose output" OFF)

add_library(bench_core STATIC)
    src/core.c
    src/util.c
    src/extra_1229.c
)

target_include_directories(bench_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(bench_core PRIVATE BENCH_VERSION="${PROJECT_VERSION}")

# generated target tool_3171
add_executable(tool_3171
    tools/tool_3171/main.cpp
)
target_link_libraries(tool_3171 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_3171 PRIVATE -Wall -Wextra)
endif()

# generated target tool_4191
add_executable(tool_4191
    tools/tool_4191/main.cpp
)
target_link_libraries(tool_4191 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_4191 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2404
add_executable(tool_2404
    tools/tool_2404/main.cpp
)
target_link_libraries(tool_2404 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2404 PRIVATE -Wall -Wextra)
endif()

# generated target tool_3044
add_executable(tool_3044
    tools/tool_3044/main.cpp
)
target_link_libraries(tool_3044 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_3044 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2438
add_executable(tool_2438
    tools/tool_2438/main.cpp
)
target_link_libraries(tool_2438 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2438 PRIVATE -Wall -Wextra)
endif()

# generated target tool_358
add_executable(tool_358
    tools/tool_358/main.cpp
)
target_link_libraries(tool_358 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_358 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2579
add_executable(tool_2579
    tools/tool_2579/main.cpp
)
target_link_libraries(tool_2579 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2579 PRIVATE -Wall -Wextra)
endif()

# generated target tool_9586
add_executable(tool_9586
    tools/tool_9586/main.cpp
)
target_link_libraries(tool_9586 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_9586 PRIVATE -Wall -Wextra)
endif()

if(BENCH_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS bench_core ARCHIVE DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)

# Sample CMake for lexer benchmarking
cmake_minimum_required(VERSION 3.16)

project(Benchmark VERSION 0.14.19 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BENCH_BUILD_TESTS "Build tests" ON)
option(BENCH_VERBOSE "Verbose output" OFF)

add_library(bench_core STATIC)
    src/core.c
    src/util.c
    src/extra_9502.c
)

target_include_directories(bench_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(bench_core PRIVATE BENCH_VERSION="${PROJECT_VERSION}")

# generated target tool_4211
add_executable(tool_4211
    tools/tool_4211/main.cpp
)
target_link_libraries(tool_4211 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_4211 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2872
add_executable(tool_2872
    tools/tool_2872/main.cpp
)
target_link_libraries(tool_2872 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2872 PRIVATE -Wall -Wextra)
endif()

# generated target tool_708
add_executable(tool_708
    tools/tool_708/main.cpp
)
target_link_libraries(tool_708 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_708 PRIVATE -Wall -Wextra)
endif()

# generated target tool_1857
add_executable(tool_1857
    tools/tool_1857/main.cpp
)
target_link_libraries(tool_1857 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_1857 PRIVATE -Wall -Wextra)
endif()

# generated target tool_7554
add_executable(tool_7554
    tools/tool_7554/main.cpp
)
target_link_libraries(tool_7554 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_7554 PRIVATE -Wall -Wextra)
endif()

# generated target tool_2964
add_executable(tool_2964
    tools/tool_2964/main.cpp
)
target_link_libraries(tool_2964 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_2964 PRIVATE -Wall -Wextra)
endif()

# generated target tool_6313
add_executable(tool_6313
    tools/tool_6313/main.cpp
)
target_link_libraries(tool_6313 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_6313 PRIVATE -Wall -Wextra)
endif()

# generated target tool_8294
add_executable(tool_8294
    tools/tool_8294/main.cpp
)
target_link_libraries(tool_8294 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_8294 PRIVATE -Wall -Wextra)
endif()

if(BENCH_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS bench_core ARCHIVE DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)

# Sample CMake for lexer benchmarking
cmake_minimum_required(VERSION 3.16)

project(Benchmark VERSION 2.1.72 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(BENCH_BUILD_TESTS "Build tests" ON)
option(BENCH_VERBOSE "Verbose output" OFF)

add_library(bench_core STATIC)
    src/core.c
    src/util.c
    src/extra_5268.c
)

target_include_directories(bench_core PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(bench_core PRIVATE BENCH_VERSION="${PROJECT_VERSION}")

# generated target tool_7443
add_executable(tool_7443
    tools/tool_7443/main.cpp
)
target_link_libraries(tool_7443 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_7443 PRIVATE -Wall -Wextra)
endif()

# generated target tool_552
add_executable(tool_552
    tools/tool_552/main.cpp
)
target_link_libraries(tool_552 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_552 PRIVATE -Wall -Wextra)
endif()

# generated target tool_3397
add_executable(tool_3397
    tools/tool_3397/main.cpp
)
target_link_libraries(tool_3397 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_3397 PRIVATE -Wall -Wextra)
endif()

# generated target tool_9143
add_executable(tool_9143
    tools/tool_9143/main.cpp
)
target_link_libraries(tool_9143 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_9143 PRIVATE -Wall -Wextra)
endif()

# generated target tool_9153
add_executable(tool_9153
    tools/tool_9153/main.cpp
)
target_link_libraries(tool_9153 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_9153 PRIVATE -Wall -Wextra)
endif()

# generated target tool_7730
add_executable(tool_7730
    tools/tool_7730/main.cpp
)
target_link_libraries(tool_7730 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_7730 PRIVATE -Wall -Wextra)
endif()

# generated target tool_5839
add_executable(tool_5839
    tools/tool_5839/main.cpp
)
target_link_libraries(tool_5839 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_5839 PRIVATE -Wall -Wextra)
endif()

# generated target tool_7166
add_executable(tool_7166
    tools/tool_7166/main.cpp
)
target_link_libraries(tool_7166 PRIVATE bench_core)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tool_7166 PRIVATE -Wall -Wextra)
endif()

if(BENCH_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

install(TARGETS bench_core ARCHIVE DESTINATION lib)
install(DIRECTORY include/ DESTINATION include)

