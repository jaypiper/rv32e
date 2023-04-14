BUILD_DIR = ./build

export PATH := $(PATH):$(abspath ./utils)

test:
	mill -i __.test

verilog:
	$(call git_commit, "generate verilog")
	mkdir -p $(BUILD_DIR)
	mill -i __.test.runMain RV32ETop -td $(BUILD_DIR)

help:
	mill -i __.test.runMain Elaborate --help

compile:
	mill -i __.compile

bsp:
	mill -i mill.bsp.BSP/install

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat

clean:
	-rm -rf $(BUILD_DIR)

.PHONY: test verilog help compile bsp reformat checkformat clean

INC_DIR += difftest
VFLAGS += $(addprefix -I, $(INC_DIR))

PWD = $(shell pwd)
VFLAGS += --exe --trace-fst --trace-underscore --timescale "1ns/1ns"
# VFLAGS += --trace-threads 4 --threads 3
VFLAGS += --autoflush -Wno-lint

CORE = $(shell find build | grep -xo .*\.v)
CSRCS = $(shell find difftest | grep -xo --extended-regexp '.*\.(cpp|c|cc)')

LDFLAGS = -ldl -O3 -Og -fPIE
CFLAGS = -Wno-deprecated-declarations -O3 -Og -pthread -fPIE -g

TRACE?=0
DIFF?=1

ifeq ($(TRACE),1)
	CFLAGS += -DTRACE
endif

NAME = top
VFLAGS += --top $(NAME)
ifeq ($(DIFF),1)
	CFLAGS += -DDIFFTEST
endif

VSRCS = $(CORE)

PROGRAM_DIR = ./bin
BIN?=add-longlong-riscv32e-nemu

compile-verilator:
	verilator $(VFLAGS) -j 8 --cc $(VSRCS) -CFLAGS "$(CFLAGS)" -LDFLAGS "$(LDFLAGS)" --exe $(CSRCS)
	make -s OPT_FAST="-O3" -j -C ./obj_dir -f V$(NAME).mk V$(NAME)

difftest:
	make compile-verilator
	time ./obj_dir/V$(NAME) $(PROGRAM_DIR)/$(BIN).bin ${mainargs}

.PHONY: difftest

-include ../Makefile
