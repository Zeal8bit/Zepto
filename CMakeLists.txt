cmake_minimum_required(VERSION 3.16)

if(NOT EXISTS "$ENV{ZOS_PATH}/cmake/zos_init.cmake")
    message(FATAL_ERROR "Set ZOS_PATH to a Zeal 8-bit OS checkout with the CMake toolchain, or build with zde cmake.")
endif()

set(ZOS_TOOLCHAIN z88dk)
include("$ENV{ZOS_PATH}/cmake/zos_init.cmake")

project(zepto LANGUAGES ASM)
find_package(Git REQUIRED)
find_program(Z88DK_DISASSEMBLER NAMES z88dk-dis z88dk.z88dk-dis)
if(NOT Z88DK_DISASSEMBLER)
    message(FATAL_ERROR "Install z88dk-dis to generate the Zepto disassembly.")
endif()

# Keep the original deployment path; objects and generated includes live in build/.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin")
add_executable(zepto src/main.asm src/model.asm src/view.asm)
set_target_properties(zepto PROPERTIES
    SUFFIX ".bin"
    LINK_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Disassemble.cmake")
target_link_options(zepto PRIVATE -m)
# The toolchain only initializes CMAKE_ASM_FLAGS when the build directory is
# first configured, so spell out the SDK include path here: an existing build
# directory created before ZOS moved its headers to sdk/z88dk-z80asm/ would
# otherwise keep the stale kernel_headers/ include path.
set(ZOS_SDK_DIR "$ENV{ZOS_PATH}/sdk/z88dk-z80asm")
target_include_directories(zepto PRIVATE "${CMAKE_CURRENT_BINARY_DIR}" "${ZOS_SDK_DIR}")

# Check Git on every build, but only reassemble when the version actually changes.
add_custom_target(zepto_version
    COMMAND "${CMAKE_COMMAND}"
        "-DGIT_EXECUTABLE=${GIT_EXECUTABLE}"
        "-DSOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
        "-DVERSION_FILE=${CMAKE_CURRENT_BINARY_DIR}/version.txt"
        -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Version.cmake"
    BYPRODUCTS "${CMAKE_CURRENT_BINARY_DIR}/version.txt"
    VERBATIM
)
add_dependencies(zepto zepto_version)
set_source_files_properties(src/view.asm PROPERTIES
    OBJECT_DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/version.txt")

# The ZOS z88dk toolchain disables automatic include dependency generation.
set(zos_headers
    "${ZOS_SDK_DIR}/zos_sys.asm"
    "${ZOS_SDK_DIR}/zos_err.asm"
    "${ZOS_SDK_DIR}/zos_keyboard.asm"
    "${ZOS_SDK_DIR}/zos_video.asm"
)
set_property(SOURCE src/main.asm src/model.asm src/view.asm APPEND PROPERTY
    OBJECT_DEPENDS ${zos_headers})

# ORG in SECTION TEXT makes z80asm emit zepto_TEXT.bin. zos_add_outputs()
# only converts SDCC/GNU output, so normalize z88dk's binary here instead.
add_custom_command(TARGET zepto POST_BUILD
    COMMAND "${CMAKE_COMMAND}" -E copy
        "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/zepto_TEXT.bin" "$<TARGET_FILE:zepto>"
    COMMAND "${CMAKE_COMMAND}"
        "-DDISASSEMBLER=${Z88DK_DISASSEMBLER}"
        "-DBINARY_FILE=$<TARGET_FILE:zepto>"
        "-DMAP_FILE=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/zepto.map"
        "-DDUMP_FILE=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/zepto.dump"
        -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Disassemble.cmake"
    BYPRODUCTS
        "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/zepto_TEXT.bin"
        "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/zepto.map"
        "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/zepto.dump"
    VERBATIM
)
