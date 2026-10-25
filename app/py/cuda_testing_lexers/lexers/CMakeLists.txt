# CMakeLists.txt Syntax Example

# 1. Project Definition and Minimum Version
# Specifies the minimum CMake version required and defines the project name.
# This should always be at the top.
cmake_minimum_required(VERSION 3.10) # Or higher, depending on features used
project(MyAwesomeApp CXX C)          # Project name, and languages used (CXX for C++, C for C)

# 2. Variables
# Set variables using 'set()'. Variables are case-sensitive.
set(CMAKE_CXX_STANDARD 17) # Request C++17 standard
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
set(CMAKE_CXX_EXTENSIONS OFF) # Disable GNU extensions for portability

set(PROJECT_VERSION_MAJOR 1)
set(PROJECT_VERSION_MINOR 0)
set(PROJECT_VERSION_PATCH 0)

# Combine version components into a single variable
set(PROJECT_VERSION "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.${PROJECT_VERSION_PATCH}")

# Variables can be used like ${VARIABLE_NAME}
message(STATUS "Building ${PROJECT_NAME} version ${PROJECT_VERSION}")

# Set common build directories (often done implicitly, but can be explicit)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# 3. Source Files and Executables/Libraries

# Define source files for the main application
set(APP_SRCS
    src/main.cpp
    src/utils.cpp
    src/logger.cpp
)

# Add an executable target
add_executable(${PROJECT_NAME} ${APP_SRCS})

# Define source files for a static library
set(MATH_LIB_SRCS
    src/math/calculator.cpp
    src/math/algebra.cpp
)
# Add a static library target
add_library(MathLib STATIC ${MATH_LIB_SRCS})

# Define source files for a shared library
set(COMM_LIB_SRCS
    src/communication/network_client.cpp
    src/communication/protocol.cpp
)
# Add a shared library target
add_library(CommLib SHARED ${COMM_LIB_SRCS})

# 4. Include Directories
# Tell CMake where to find header files.
# PUBLIC: Consumers of this target also need this include path.
# PRIVATE: Only this target needs this include path.
# INTERFACE: Only consumers of this target need this include path.
target_include_directories(${PROJECT_NAME} PUBLIC
    ${CMAKE_SOURCE_DIR}/include # Global project headers
)
target_include_directories(MathLib PUBLIC
    ${CMAKE_SOURCE_DIR}/src/math # Headers specific to MathLib
)
target_include_directories(CommLib PUBLIC
    ${CMAKE_SOURCE_DIR}/src/communication # Headers specific to CommLib
)

# 5. Linking Libraries
# Link targets against other libraries or CMake targets.
target_link_libraries(${PROJECT_NAME}
    PRIVATE # Only MyAwesomeApp needs MathLib
        MathLib
    PUBLIC # MyAwesomeApp and its consumers need CommLib (e.g., if CommLib provides an API)
        CommLib
)

# Example of linking against a system library (e.g., pthreads on Linux)
# target_link_libraries(${PROJECT_NAME} PRIVATE Threads::Threads) # More modern CMake for pthreads

# 6. Compiler Definitions and Flags
# Add preprocessor definitions and compiler flags.
# Definitions: -DDEBUG_MODE, -D_MY_APP_VERSION="1.0.0"
target_compile_definitions(${PROJECT_NAME} PRIVATE
    DEBUG_MODE # Add -DDEBUG_MODE when building MyAwesomeApp
    APP_VERSION="${PROJECT_VERSION}" # Example: -DAPP_VERSION="1.0.0"
)

# Compiler options/flags
target_compile_options(${PROJECT_NAME} PRIVATE
    -Werror # Treat warnings as errors for this target
)

# Add global compiler flags (less common, usually use target_compile_options)
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")

# 7. Conditional Logic
# Use if/endif/else for conditional compilation or configuration.
if(WIN32) # Built-in CMake variable for Windows platform
    message(STATUS "Configuring for Windows platform...")
    target_link_libraries(${PROJECT_NAME} PRIVATE ws2_32) # Link against WinSock on Windows
elseif(UNIX) # Built-in CMake variable for Unix-like platforms (Linux, macOS, BSD)
    message(STATUS "Configuring for Unix-like platform...")
    # Example: link against specific library only on Unix-like systems
    # target_link_libraries(${PROJECT_NAME} PRIVATE dl)
else()
    message(WARNING "Unknown platform detected. Some features might not be available.")
endif()

# 8. Finding and Using External Packages
# Use find_package() to locate external libraries and tools.
# CMake provides Find modules for many common libraries (e.g., Boost, ZLIB, OpenSSL).
# Requires: Boost installed and discoverable (e.g., via BOOST_ROOT environment variable)
find_package(Boost 1.70 COMPONENTS system filesystem REQUIRED)
if(Boost_FOUND)
    message(STATUS "Found Boost version ${Boost_VERSION}")
    # Link against Boost libraries
    target_link_libraries(${PROJECT_NAME} PRIVATE Boost::system Boost::filesystem)
    # Add Boost include directories
    # target_include_directories(${PROJECT_NAME} PRIVATE ${Boost_INCLUDE_DIRS}) # Old way
    # Modern CMake often adds these automatically when linking with Boost::component
else()
    message(FATAL_ERROR "Boost library not found. Please install it or set BOOST_ROOT.")
endif()

# 9. Installation Rules
# Define where to install the built artifacts.
install(TARGETS ${PROJECT_NAME} MathLib CommLib
    RUNTIME DESTINATION bin        # Executables go into bin
    ARCHIVE DESTINATION lib       # Static libraries go into lib
    LIBRARY DESTINATION lib       # Shared libraries go into lib
)

# Install header files
install(DIRECTORY ${CMAKE_SOURCE_DIR}/include/ DESTINATION include)

# Install documentation (example)
install(FILES docs/README.md DESTINATION share/doc/${PROJECT_NAME})

# 10. Subdirectories
# Include other CMakeLists.txt files from subdirectories.
# This promotes modularity for larger projects.
add_subdirectory(tests) # Assumes 'tests/CMakeLists.txt' exists

# 11. Custom Commands (Advanced)
# Add custom build steps.
# add_custom_command(
#     OUTPUT ${CMAKE_BINARY_DIR}/generated_code.h
#     COMMAND python ${CMAKE_SOURCE_DIR}/scripts/generate_code.py > ${CMAKE_BINARY_DIR}/generated_code.h
#     DEPENDS ${CMAKE_SOURCE_DIR}/scripts/generate_code.py
#     COMMENT "Generating code..."
# )
# add_custom_target(generate_code ALL DEPENDS ${CMAKE_BINARY_DIR}/generated_code.h)
# target_sources(${PROJECT_NAME} PRIVATE ${CMAKE_BINARY_DIR}/generated_code.h)

# 12. CTest for Testing (in tests/CMakeLists.txt)
# enable_testing()
# add_test(NAME MyMathTest COMMAND ${PROJECT_NAME}_test) # Assuming a test executable