
#ifndef DIFFTEST_H
#define DIFFTEST_H
#include <dlfcn.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <csetjmp>
#include <vector>
#include "../obj_dir/Vtop.h"

#include <iostream>

typedef struct CPU_STATE{
    uint32_t gpr[32];
    uint32_t pc;
    uint32_t csr[0x1000];
    uint32_t priv;
    uint32_t inst;
    int valid;
    int is_mmio;
}CPU_state;

extern "C" void flash_memcpy(uint8_t* src, size_t len);

void get_regs(CPU_state* state);
extern "C" void update_reg(int id, int val);


#define TRACE_START 0



#define SEPC_LAB      0x0
#define STVEC_LAB     0x1
#define SCAUSE_LAB    0x2
#define STVAL_LAB     0x3
#define SSCRATCH_LAB  0x4
#define SSTATUS_LAB   0x5
#define SATP_LAB      0x6
#define MTVEC_LAB     0x7
#define MEPC_LAB      0x8
#define MCAUSE_LAB    0x9
#define MIE_LAB       0xa
#define MIP_LAB       0xb
#define MTVAL_LAB     0xc
#define MSCRATCH_LAB  0xd
#define MSTATUS_LAB   0xe
#define MHARTID_LAB   0xf

#define SEPC_ID       0x141
#define STVEC_ID      0x105
#define SCAUSE_ID     0x142
#define STVAL_ID      0x143
#define SSCRATCH_ID   0x140
#define SSTATUS_ID    0x100
#define SATP_ID       0x180
#define SIE_ID        0x104
#define SIP_ID        0x144
#define MTVEC_ID      0x305
#define MEPC_ID       0x341
#define MCAUSE_ID     0x342
#define MIE_ID        0x304
#define MIP_ID        0x344
#define MTVAL_ID      0x343
#define MSCRATCH_ID   0x340
#define MSTATUS_ID    0x300
#define MEDELEG_ID    0x302
#define MIDELEG_ID    0x303
#define MHARTID       0xf14
#define USCRATCH      0x40
#define MISA          0x301

#define MIP_MASK      0x2

#endif