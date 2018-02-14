cmake_minimum_required(VERSION 3.9)

project(cbullet VERSION 1.0 DESCRIPTION "C bindings to the bullet physics library")

include(GNUInstallDirs REQUIRED)

find_library(B3COMMON_LIB Bullet3Common)
find_library(B3DYNAMICS_LIB Bullet3Dynamics)
find_library(B3COLLISION_LIB Bullet3Collision)
find_library(B3GEOMETRY_LIB Bullet3Geometry)
find_library(BDYNAMICS_LIB BulletDynamics)
find_library(BCOLLISION_LIB BulletCollision)
find_library(BOPENCL_LIB Bullet3OpenCL_clew)
find_library(LINEAR_MATH_LIB LinearMath)
find_library(BFILE_LOADER_LIB BulletFileLoader)
find_library(BWORLD_IMPORTER_LIB BulletWorldImporter)
find_library(BINVERSE_DYN_LIB BulletInverseDynamics)
find_library(BSOFT_BODY_LIB BulletSoftBody)
find_library(CONVEX_DECOMP_LIB ConvexDecomposition)
find_library(GIMPACT_LIB GIMPACTUtils)
find_library(GACD_LIB HACD)
add_library(cbullet SHARED
  cbullet.cpp)
target_link_libraries(cbullet "${B3COMMON_LIB}")
target_link_libraries(cbullet "${B3DYNAMICS_LIB}")
target_link_libraries(cbullet "${B3COLLISION_LIB}")
target_link_libraries(cbullet "${B3GEOMETRY_LIB}")
target_link_libraries(cbullet "${BDYNAMICS_LIB}")
target_link_libraries(cbullet "${BCOLLISION_LIB}")
target_link_libraries(cbullet "${LINEAR_MATH_LIB}")
target_link_libraries(cbullet "${BFILE_LOADER_LIB}")
target_link_libraries(cbullet "${BWORLD_IMPORTER_LIB}")
target_link_libraries(cbullet "${BOPENCL_LIB}")
target_link_libraries(cbullet "${BINVERSE_DYN_LIB}")
target_link_libraries(cbullet "${BSOFT_BODY_LIB}")
target_link_libraries(cbullet "${CONVEX_DECOMP_LIB}")
target_link_libraries(cbullet "${GIMPACT_LIB}")
target_link_libraries(cbullet "${HACD_LIB}")

set_target_properties(cbullet PROPERTIES VERSION ${PROJECT_VERSION})
set_target_properties(cbullet PROPERTIES SOVERSION 1)

set_target_properties(cbullet PROPERTIES PUBLIC_HEADER cbullet.h)

install(TARGETS cbullet
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
