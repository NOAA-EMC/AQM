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

include("aqm_files.cmake")

# src/shr
add_library(shr OBJECT ${aqm_shr_files})
add_dependencies(shr drv aqmio ioapi CCTM)
set_target_properties(shr PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(shr PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>)
target_compile_definitions(shr PUBLIC "-DSUBST_CONST=\"CONST.EXT\" -DSUBST_FILES_ID=\"FILES_CTM.EXT\"")
target_link_libraries(shr PRIVATE drv aqmio ioapi CCTM)
target_link_libraries(shr PRIVATE esmf)

# src/drv
add_library(drv OBJECT ${aqm_drv_files})
add_dependencies(drv shr CCTM aqmio ioapi)
set_target_properties(drv PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(drv PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>)
target_compile_definitions(drv PUBLIC "-Dverbose_driver")
target_link_libraries(drv PRIVATE shr CCTM aqmio ioapi)
target_link_libraries(drv PRIVATE esmf)

# src/io/aqmio
add_library(aqmio OBJECT ${aqm_aqmio_files})
set_target_properties(aqmio PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(aqmio PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>)
target_link_libraries(aqmio PRIVATE esmf)

# src/io/ioapi
add_library(ioapi OBJECT ${aqm_ioapi_files})
set_target_properties(ioapi PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(ioapi PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>)
target_include_directories(ioapi PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/model/CMAQ/CCTM/src/ICL/fixed/filenames)
target_compile_definitions(ioapi PUBLIC "-DSUBST_FILES_ID=\"FILES_CTM.EXT\"")
target_link_libraries(ioapi PRIVATE esmf)

# src/model
add_library(CCTM OBJECT ${aqm_CCTM_files})
add_dependencies(CCTM shr aqmio ioapi)
set_target_properties(CCTM PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(CCTM PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>)
target_include_directories(ioapi PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src/model/CMAQ/CCTM/src/ICL/fixed/filenames
                                         ${CMAKE_CURRENT_SOURCE_DIR}/src/model/CMAQ/CCTM/src/ICL/fixed/const
                                         ${CMAKE_CURRENT_SOURCE_DIR}/src/model/CMAQ/CCTM/src/ICL/fixed/emctrl
                                         ${CMAKE_CURRENT_SOURCE_DIR}/src/model/CMAQ/CCTM/src/ICL/fixed/mpi)
target_compile_definitions(CCTM PUBLIC "-DSUBST_FILES_ID=\"FILES_CTM.EXT\""
                                       "-DSUBST_CONST=\"CONST.EXT\""
                                       "-DSUBST_EMISPRM=\"EMISPRM.EXT\""
                                       "-DSUBST_PE_COMM=\"PE_COMM.EXT\""
                                       "-DSUBST_COMM=NOOP_COMM"
                                       "-DSUBST_BARRIER=NOOP_BARRIER"
                                       "-DSUBST_SUBGRID_INDEX=NOOP_SUBGRID_INDEX"
                                       "-DMOSAIC_MOD=MOSAIC_MODULE -DMosaic_Mod=Mosaic_Module"
                                       "-DEDDYX=DUMMY_EDDYX"
                                       "-DOPCONC=DUMMY_OPCONC -DOPACONC=DUMMY_OPACONC"
                                       "-DOPWDEP=DUMMY_OPWDEP -DWR_INIT=DUMMY_WR_INIT"
                                       "-Dverbose_aero -Dverbose_gas"
                                       "-Dmpas -D_AQM_")
target_link_libraries(CCTM PRIVATE shr aqmio ioapi)
target_link_libraries(CCTM PRIVATE esmf)

# AQM
add_library(aqm STATIC $<TARGET_OBJECTS:shr>
                         $<TARGET_OBJECTS:drv>
                         $<TARGET_OBJECTS:aqmio>
                         $<TARGET_OBJECTS:ioapi>
                         $<TARGET_OBJECTS:CCTM>)
add_library(aqm::aqm ALIAS aqm)
target_include_directories(aqm PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                      $<INSTALL_INTERFACE:mod>)
target_link_libraries(aqm PUBLIC esmf)
list(APPEND LIB_TARGETS aqm)

###############################################################################
### Install
###############################################################################

install(
  TARGETS   ${LIB_TARGETS}
  EXPORT    AQMExports
  LIBRARY   DESTINATION lib
  ARCHIVE   DESTINATION lib
  COMPONENT Library)

export(EXPORT    AQMExports
       NAMESPACE aqm::
       FILE      aqm-targets.cmake)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod DESTINATION ${CMAKE_INSTALL_PREFIX}/CDEPS)

install(EXPORT      AQMExports
        NAMESPACE   aqm::
        FILE        aqm-targets.cmake
        DESTINATION lib/cmake)

# # create target
# add_library(aqm OBJECT ${aqm_files})
# set_property(TARGET aqm PROPERTY IMPORTED_LOCATION ${aqm_dir}/libaqm.a)
# add_dependencies(aqm aqm_gnu)
# target_include_directories(aqm INTERFACE ${aqm_dir})
# target_link_libraries(aqm INTERFACE esmf NetCDF::NetCDF_Fortran)
