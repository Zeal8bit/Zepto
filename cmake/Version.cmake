execute_process(
    COMMAND "${GIT_EXECUTABLE}" describe --always
    WORKING_DIRECTORY "${SOURCE_DIR}"
    OUTPUT_VARIABLE version
    OUTPUT_STRIP_TRAILING_WHITESPACE
    RESULT_VARIABLE result
)
if(NOT result EQUAL 0)
    message(FATAL_ERROR "Could not determine the Zepto Git version")
endif()

# Preserve the trailing space used by the title bar, without a newline.
set(version "${version} ")
if(EXISTS "${VERSION_FILE}")
    file(READ "${VERSION_FILE}" previous_version)
    if(previous_version STREQUAL version)
        return()
    endif()
endif()
file(WRITE "${VERSION_FILE}" "${version}")
