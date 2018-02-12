cmake_minimum_required(VERSION 3.9)

project(cbullet VERSION 1.0 DESCRIPTION "C bindings to the bullet physics library")

include(FindPkgConfig REQUIRED)
include(GNUInstallDirs REQUIRED)
pkg_check_modules(BULLET REQUIRED bullet)

add_library(cbullet SHARED
  cbullet.cpp)

set_target_properties(cbullet PROPERTIES VERSION ${PROJECT_VERSION})
set_target_properties(cbullet PROPERTIES SOVERSION 1)

set_target_properties(cbullet PROPERTIES PUBLIC_HEADER cbullet.h)

install(TARGETS cbullet
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
