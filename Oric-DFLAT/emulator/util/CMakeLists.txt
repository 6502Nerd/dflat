# CMakeList.txt : Top-level CMake project file, do global configuration
# and include sub-projects here.
#
cmake_minimum_required (VERSION 3.8)

project ("dflat-utils")

# Include sub-projects.
add_subdirectory ("dftap2txt")
add_subdirectory ("dftap2wav")
add_subdirectory ("dftxt2tap")
