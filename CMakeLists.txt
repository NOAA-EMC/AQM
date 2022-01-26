cmake_minimum_required (VERSION 3.18)

project (AQM
         VERSION 0.2.0
         LANGUAGES Fortran)

# check dependencies if this is built as a CMake subproject
# GNU configure will check the dependencies if built as standalone project
get_directory_property(isSubproject PARENT_DIRECTORY)
if (isSubproject)
  if (NOT ESMF_FOUND)
    find_package (ESMF REQUIRED)
  endif ()
  if (NOT NetCDF_Fortran_FOUND)
    find_package (NetCDF REQUIRED Fortran)
  endif ()
endif ()

# Run configure/make as external project
include (ExternalProject)

# set install directory
set (aqm_dir ${CMAKE_CURRENT_BINARY_DIR}/aqm)

# add configure flag for debug build type
if ("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
   set (configure_options "--enable-debug")
endif ()

ExternalProject_Add (aqm_gnu
  PREFIX             aqm
  DOWNLOAD_COMMAND   ""
  CONFIGURE_COMMAND  ${CMAKE_CURRENT_SOURCE_DIR}/configure
                         --prefix=${aqm_dir} --libdir=${aqm_dir} --includedir=${aqm_dir}
                         FC=${CMAKE_Fortran_COMPILER} F77=${CMAKE_Fortran_COMPILER} CC=${CMAKE_C_COMPILER}
                         ${configure_options}
)

# create target
add_library(aqm STATIC IMPORTED GLOBAL)
set_property(TARGET aqm PROPERTY IMPORTED_LOCATION ${aqm_dir}/libaqm.a)
add_dependencies(aqm aqm_gnu)
target_include_directories(aqm INTERFACE ${aqm_dir})
target_link_libraries(aqm INTERFACE esmf NetCDF::NetCDF_Fortran)
