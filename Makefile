SHELL := /bin/bash

SRCS = main.asm model.asm view.asm
BIN = zepto.bin

# Directory where source files are and where the binaries will be put
INPUT_DIR = src
OUTPUT_DIR = bin

# Include directory containing Zeal 8-bit OS header file.
ifndef ZOS_PATH
$(error "Please define ZOS_PATH environment variable. It must point to Zeal 8-bit OS source code path.")
endif

ZOS_INCLUDE = $(ZOS_PATH)/kernel_headers/z88dk-z80asm/

# Assembler binary name
ASM = z88dk-z80asm
# Assembler flags
ASMFLAGS = -m -b -I$(ZOS_INCLUDE) -O$(OUTPUT_DIR)

# Mark version.txt as PHONY to force generating it every time
.PHONY: all version.txt

all: version.txt $(OUTPUT_DIR) $(BIN)

version.txt:
	git describe --always | tr "\n" " " > $@

$(BIN): $(addprefix $(INPUT_DIR)/, $(SRCS))
	$(ASM) $(ASMFLAGS) -o$@ $^
	mv $(OUTPUT_DIR)/*_TEXT.bin $(OUTPUT_DIR)/$@
	z88dk.z88dk-dis -mz80 -o 0x4000 -x $(OUTPUT_DIR)/zepto.map $(OUTPUT_DIR)/$@ > $(OUTPUT_DIR)/zepto.dump

$(OUTPUT_DIR):
	mkdir -p $@

clean:
	rm -rf bin/ version.txt