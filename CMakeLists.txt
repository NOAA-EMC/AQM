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

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "-ffree-line-length-none -ffixed-line-length-none")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS "-extend-source 132")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -check all -check noarg_temp_created -fpe0 -fno-alias -ftrapuv -traceback")
  # set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -check bounds -check uninit -fpe0 -fno-alias -ftrapuv -traceback")
else()
  message(WARNING "Fortran compiler with ID ${CMAKE_Fortran_COMPILER_ID} will be used with CMake default options")
endif()

include("aqm_files.cmake")

# src/shr
add_library(shr OBJECT ${aqm_shr_files})
add_dependencies(shr ioapi aqmio)
set_target_properties(shr PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(shr PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                       $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/model/CMAQ/CCTM/src/ICL/fixed/filenames>
                                       $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/model/CMAQ/CCTM/src/ICL/fixed/const>
				       $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/src/io/ioapi>
				       $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/src/io/aqmio>
				       $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/src/model>)
target_compile_definitions(shr PUBLIC SUBST_CONST="CONST.EXT" 
				      SUBST_FILES_ID="FILES_CTM.EXT")
target_link_libraries(shr PRIVATE esmf)

# src/drv
add_library(drv OBJECT ${aqm_drv_files})
add_dependencies(drv shr)
set_target_properties(drv PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(drv PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                       $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/src/shr>
                                       $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/src/io/ioapi>
                                       $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/src/io/aqmio>
                                       $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/src/model>)
target_compile_definitions(drv PUBLIC verbose_driver)
#target_link_libraries(drv PRIVATE shr CCTM esmf)
target_link_libraries(drv PRIVATE shr CCTM)

# src/io/aqmio
add_library(aqmio OBJECT ${aqm_aqmio_files})
set_target_properties(aqmio PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(aqmio PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>)
target_link_libraries(aqmio PRIVATE esmf)

# src/io/ioapi
add_library(ioapi OBJECT ${aqm_ioapi_files})
set_target_properties(ioapi PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(ioapi PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                         $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/model/CMAQ/CCTM/src/ICL/fixed/filenames>)
target_compile_definitions(ioapi PUBLIC SUBST_FILES_ID="FILES_CTM.EXT")
target_link_libraries(ioapi PRIVATE esmf)

# custom targets
add_custom_target(aqm_mosaic
                  COMMAND ${CMAKE_COMMAND} -E create_symlink
                          ${CMAKE_CURRENT_BINARY_DIR}/mod/mosaic_module.mod
                          ${CMAKE_CURRENT_BINARY_DIR}/mod/mosaic_mod.mod
                  )

# src/model
add_library(CCTM OBJECT ${aqm_CCTM_files})
add_dependencies(CCTM shr aqmio ioapi aqm_mosaic)
set_target_properties(CCTM PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(CCTM PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/model/CMAQ/CCTM/src/ICL/fixed/filenames>
                                        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/model/CMAQ/CCTM/src/ICL/fixed/const>
                                        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/model/CMAQ/CCTM/src/ICL/fixed/emctrl>
                                        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/model/CMAQ/CCTM/src/ICL/fixed/mpi>
					$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/src/shr>
                                        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/src/io/ioapi>
                                        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/src/io/aqmio>)
target_compile_definitions(CCTM PUBLIC SUBST_FILES_ID="FILES_CTM.EXT"
                                       SUBST_CONST="CONST.EXT"
                                       SUBST_EMISPRM="EMISPRM.EXT"
                                       SUBST_PE_COMM="PE_COMM.EXT"
                                       SUBST_COMM=NOOP_COMM
                                       SUBST_BARRIER=NOOP_BARRIER
                                       SUBST_HI_LO_BND_PE=NOOP_HI_LO_BND_PE
                                       SUBST_SUBGRID_INDEX=NOOP_SUBGRID_INDEX
                                       AQCHEM=DUMMY_AQCHEM
                                       CONVCLD_ACM=DUMMY_CONVCLD_ACM
                                       EDDYX=DUMMY_EDDYX
                                       MOSAIC_MOD=MOSAIC_MODULE
                                       Mosaic_Mod=Mosaic_Module
                                       OPCONC=DUMMY_OPCONC
				       OPACONC=DUMMY_OPACONC
                                       OPWDEP=DUMMY_OPWDEP
				       WR_INIT=DUMMY_WR_INIT
                                       verbose_aero
				       verbose_gas
                                       verbose_cio
                                      # mpas
				       _AQM_)

# AQM
add_library(aqm STATIC ${aqm_files} $<TARGET_OBJECTS:shr>
                         	    $<TARGET_OBJECTS:drv>
                         	    $<TARGET_OBJECTS:aqmio>
                         	    $<TARGET_OBJECTS:ioapi>
                         	    $<TARGET_OBJECTS:CCTM>)
set_target_properties(aqm PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
add_library(aqm::aqm ALIAS aqm)
target_include_directories(aqm PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                      $<INSTALL_INTERFACE:mod>)
target_link_libraries(aqm PUBLIC esmf NetCDF::NetCDF_Fortran)
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

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod DESTINATION ${CMAKE_INSTALL_PREFIX}/AQM)

install(EXPORT      AQMExports
        NAMESPACE   aqm::
        FILE        aqm-targets.cmake
        DESTINATION lib/cmake)
