execute_process(
    COMMAND "${DISASSEMBLER}" -mz80 -x "${MAP_FILE}" -o 0x4000 "${BINARY_FILE}"
    OUTPUT_FILE "${DUMP_FILE}"
    RESULT_VARIABLE result
)
if(NOT result EQUAL 0)
    message(FATAL_ERROR "Could not disassemble ${BINARY_FILE}")
endif()
