cmake_minimum_required(VERSION 3.1)
project(DemoTheWayThingsCouldBe Fortran)

################################################################################
# The way things are model
file(GLOB DEMO_SRCS src/*.F90)

add_executable(host_model ${DEMO_SRCS})

set_target_properties(host_model PROPERTIES OUTPUT_NAME host_model)

################################################################################
