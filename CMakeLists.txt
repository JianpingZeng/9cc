cmake_minimum_required(VERSION 2.8)

set(UTILS utils/)
set(SYS sys/)

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
  initializer.c
  ir.c
  block.c
)

set(SYS_SRC ${SYS}linux.c)
set(MCC_SRC mcc.c)

add_definitions(-DBUILD_DIR="${CMAKE_SOURCE_DIR}")

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  add_definitions(-DCONFIG_COLOR_TERM)
  add_definitions(-DCONFIG_DARWIN)
  add_compile_options(-Wall -std=c99)
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
elseif (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  add_definitions(-DCONFIG_COLOR_TERM)
  add_definitions(-DCONFIG_LINUX)
  add_compile_options(-Wall -std=c99)
  set(LINK_LIBRARIES unwind)
elseif (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
  add_definitions(-DCONFIG_WINDOWS)
else()
  message(FATAL_ERROR "unknown platform")
endif()

add_executable(mcc ${MCC_SRC} ${CC1_SRC} ${UTILS_SRC} ${SYS_SRC})
target_link_libraries(mcc ${LINK_LIBRARIES})

## Testing
enable_testing()
set(TEST test/)
set(TEST_MAIN_SRC ${TEST}main.c)
file(GLOB TEST_OBJ_SRC ${TEST}test_*.c)
foreach (obj_src ${TEST_OBJ_SRC})
  string(REGEX REPLACE ".c$" ".bin" obj_bin ${obj_src})
  get_filename_component(bin ${obj_bin} NAME)
  add_executable(${bin} ${obj_src} ${TEST_MAIN_SRC} ${UTILS_SRC})
  add_test(NAME ${bin} COMMAND ${bin} -V)
endforeach()
