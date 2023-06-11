#include "svdpi.h"
#include "Vtop__Dpi.h"
#include "difftest.h"

extern CPU_state state;

extern "C" void update_reg(int id,int val){
    state.gpr[id] = val;
}

extern "C" void update_pc(int valid, int pc, int inst, int isMMIO){
    state.pc = (uint32_t)pc;
    state.inst = (uint32_t)inst;
    state.valid = valid;
    state.is_mmio = isMMIO;
}

extern "C" void update_csr(int id, long long val){
    state.csr[id] = val;
}
