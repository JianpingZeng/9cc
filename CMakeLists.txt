cmake_minimum_required(VERSION 2.8)

set(UTILS utils/)
set(SYS sys/)
set(TARGET 7cc)
set(CC1 cc1)

set(UTILS_SRC
  ${UTILS}wrapper.c
  ${UTILS}strbuf.c
  ${UTILS}vector.c
  ${UTILS}map.c
  ${UTILS}string.c
  ${UTILS}hideset.c
  ${UTILS}set.c
)

set(CC1_SRC
  alloc.c
  ast.c
  cc.c
  cpp.c
  print.c
  decl.c
  error.c
  eval.c
  expr.c
  gen.c
  lex.c
  stmt.c
  sym.c
  type.c
  input.c
  ir.c
  block.c
)

set(7CC_SRC 7cc.c)

add_definitions(-DBUILD_DIR="${CMAKE_SOURCE_DIR}")

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
   ## Mac OS X

  add_definitions(-DCONFIG_COLOR_TERM)
  add_definitions(-DCONFIG_DARWIN)
  add_compile_options(-Wall -std=c99 -g)
  execute_process(COMMAND
        "xcrun" "--show-sdk-path"
        OUTPUT_VARIABLE XCODE_SDK_DIR
        OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  execute_process(COMMAND
        "xcrun" "--show-sdk-version"
        OUTPUT_VARIABLE OSX_SDK_VERSION
        OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  add_definitions(-DXCODE_DIR="${XCODE_SDK_DIR}")
  add_definitions(-DOSX_SDK_VERSION="${OSX_SDK_VERSION}")
  set(SYS_SRC ${SYS}unix.c ${SYS}darwin.c)
  
elseif (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  ## Linux

  add_definitions(-DCONFIG_COLOR_TERM)
  add_definitions(-DCONFIG_LINUX)
  add_compile_options(-Wall -std=c99 -g)
  set(LINK_LIBRARIES unwind)
  set(SYS_SRC ${SYS}unix.c ${SYS}linux.c)
  
elseif (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
  ## Windows

  add_definitions(-DCONFIG_WINNT)
  set(SYS_SRC ${SYS}winnt.c)
  set(LINK_LIBRARIES Shlwapi Rpcrt4)
  ## Guess VCINSTALLDIR
  if ("$ENV{VCINSTALLDIR}" STREQUAL "")
     string(REGEX REPLACE "/bin/[^/]*$" "" VCINSTALLDIR "${CMAKE_C_COMPILER}")
  else()
     set(VCINSTALLDIR $ENV{VCINSTALLDIR})
  endif()
  file(GLOB bat ${VCINSTALLDIR}/vcvarsall.bat)
  ##TODO: run vcvarsall.bat...
  ##
else()
  message(FATAL_ERROR "unknown platform")
endif()

# 7cc
add_executable(${TARGET} ${7CC_SRC} ${UTILS_SRC} ${SYS_SRC})
target_link_libraries(${TARGET} ${LINK_LIBRARIES})

# cc1
add_executable(${CC1} ${CC1_SRC} ${UTILS_SRC} ${SYS_SRC})
target_link_libraries(${CC1} ${LINK_LIBRARIES})

# stage1

# stage2

# stage3

## Testing
enable_testing()
set(TEST test/)
set(TEST_MAIN_SRC ${TEST}main.c)
file(GLOB TEST_OBJ_SRC ${TEST}test_*.c)
foreach (obj_src ${TEST_OBJ_SRC})
  string(REGEX REPLACE ".c$" ".bin" obj_bin ${obj_src})
  get_filename_component(bin ${obj_bin} NAME)
  add_executable(${bin} ${obj_src} ${TEST_MAIN_SRC} ${UTILS_SRC})
  add_test(NAME ${bin} COMMAND ${bin})
endforeach()
