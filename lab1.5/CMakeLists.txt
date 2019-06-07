cmake_minimum_required(VERSION 3.14)
project(lab1.5 Fortran)


enable_language(Fortran)
set(SCMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)
add_executable(lab1.5 src/main.f90 src/io.f90 src/process.f90 src/environment.f90)