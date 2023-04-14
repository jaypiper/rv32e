package params

import chisel3._
import chisel3.util._
import common._
object common {
  val REG_WIDTH = 32
  val REG_NUM = 16
  val REG_NUM_WIDTH = log2Up(REG_NUM)
  val ADDR_WIDTH = 32
  val INST_WIDTH = 32
  val CSR_NUM_WIDTH = 12
  val PC_START = "h80000000".U(ADDR_WIDTH.W)
}

trait Insts{
    //mv U-type           | 28| 24| 20| 16| 12|  8|  4|  0|
    def LUI     = BitPat("b?????????????????????????0110111")
    def AUIPC   = BitPat("b?????????????????????????0010111")
    // jmp J-type
    def JAL     = BitPat("b?????????????????????????1101111")
    def JALR    = BitPat("b?????????????????000?????1100111")
    // Branch B-type
    def BEQ     = BitPat("b?????????????????000?????1100011")
    def BNE     = BitPat("b?????????????????001?????1100011")
    def BLT     = BitPat("b?????????????????100?????1100011")
    def BGE     = BitPat("b?????????????????101?????1100011")
    def BLTU    = BitPat("b?????????????????110?????1100011")
    def BGEU    = BitPat("b?????????????????111?????1100011") 
    //load I-type
    def LB      = BitPat("b?????????????????000?????0000011")
    def LH      = BitPat("b?????????????????001?????0000011")
    def LW      = BitPat("b?????????????????010?????0000011")
    def LBU     = BitPat("b?????????????????100?????0000011")
    def LHU     = BitPat("b?????????????????101?????0000011")
    //store S-type
    def SB      = BitPat("b?????????????????000?????0100011")
    def SH      = BitPat("b?????????????????001?????0100011")
    def SW      = BitPat("b?????????????????010?????0100011")
    // I-type
    def ADDI    = BitPat("b?????????????????000?????0010011")
    def SLTI    = BitPat("b?????????????????010?????0010011")
    def SLTIU   = BitPat("b?????????????????011?????0010011")
    def XORI    = BitPat("b?????????????????100?????0010011")
    def ORI     = BitPat("b?????????????????110?????0010011")
    def ANDI    = BitPat("b?????????????????111?????0010011")
    def SLLI    = BitPat("b000000???????????001?????0010011")
    def SRLI    = BitPat("b000000???????????101?????0010011")
    def SRAI    = BitPat("b010000???????????101?????0010011")
    // R-type
    def ADD     = BitPat("b0000000??????????000?????0110011")
    def SUB     = BitPat("b0100000??????????000?????0110011")
    def SLL     = BitPat("b0000000??????????001?????0110011")
    def SLT     = BitPat("b0000000??????????010?????0110011")
    def SLTU    = BitPat("b0000000??????????011?????0110011")
    def XOR     = BitPat("b0000000??????????100?????0110011")
    def SRL     = BitPat("b0000000??????????101?????0110011")
    def SRA     = BitPat("b0100000??????????101?????0110011")
    def OR      = BitPat("b0000000??????????110?????0110011")
    def AND     = BitPat("b0000000??????????111?????0110011")
    // CSR I-type
    def CSRRW   = BitPat("b?????????????????001?????1110011")
    def CSRRS   = BitPat("b?????????????????010?????1110011")
    def CSRRC   = BitPat("b?????????????????011?????1110011")
    def CSRRWI  = BitPat("b?????????????????101?????1110011")
    def CSRRSI  = BitPat("b?????????????????110?????1110011")
    def CSRRCI  = BitPat("b?????????????????111?????1110011")

    def ECALL   = BitPat("b00000000000000000000000001110011")
    def SRET    = BitPat("b00010000001000000000000001110011")
    def MRET    = BitPat("b00110000001000000000000001110011")
    //nemu_trap
    def TRAP    = BitPat("b00000000000000000000000001101011")
    def FENCE_I = BitPat("b00000000000000000001000000001111")
    def FENCE   = BitPat("b0000????????00000000000000001111")
    def SFENCE_VMA  = BitPat("b0001001??????????000000001110011")

    def WFI     = BitPat("b00010000010100000000000001110011")

}

trait DeType{
    val EMPTY = 0.U(3.W)
    val RType = 1.U(3.W)
    val IType = 2.U(3.W)
    val SType = 3.U(3.W)
    val BType = 4.U(3.W)
    val UType = 5.U(3.W)
    val JType = 6.U(3.W)
    val INVALID  = 7.U(3.W)

    val c_invalid :: cr :: ci :: css :: ciw :: cl :: cs :: cb :: cj :: Nil = Enum(9)
    val (no_imm :: ciw_u :: cls_w :: cls_d :: ci_u :: ci_s :: ci_u2 ::
         ci_u3 :: ci_s4 :: ci_s12 :: cj_s1 :: cb_s1 :: css_u2 :: css_u3 :: Nil) = Enum(14)
}

trait ALUOP{
    val (alu_NOP    :: alu_MV1  :: alu_MV2  :: alu_ADD  :: alu_XOR   ::
         alu_OR     :: alu_AND  :: alu_SLL  :: alu_SRL  :: alu_SRA   ::
         alu_SUB    :: alu_SLT  :: alu_SLTU :: alu_MUL  :: alu_MULH  ::
         alu_MULHU  :: alu_MULHSU :: alu_DIV  :: alu_DIVU :: alu_REM ::
         alu_REMU   :: alu_NAND   :: Nil) = Enum(22)
    val ALUOP_WIDTH = 5
}

trait BrType{
    val bEQ  = 0.U(3.W)
    val bNE  = 1.U(3.W)
    val bLT  = 4.U(3.W)
    val bGE  = 5.U(3.W)
    val bLTU = 6.U(3.W)
    val bGEU = 7.U(3.W)
}

trait MemMode {
  val mode_NOP = "b00000".U(5.W)
  val mode_LB  = "b00100".U(5.W)
  val mode_LH  = "b00101".U(5.W)
  val mode_LW  = "b00110".U(5.W)
  val mode_LBU = "b10100".U(5.W)
  val mode_LHU = "b10101".U(5.W)
  val mode_SB  = "b01000".U(5.W)
  val mode_SH  = "b01001".U(5.W)
  val mode_SW  = "b01010".U(5.W)

  def rdata_by_mode(mode: UInt, rdata32: UInt) = {
        MuxLookup(mode, 0.U, Seq(  // can take advantage of the encoding of dc_mode
            mode_LB  -> Cat(Fill(32 - 8, rdata32(7)), rdata32(7, 0)),
            mode_LBU -> rdata32(7, 0).asUInt,
            mode_LH  -> Cat(Fill(32 - 16, rdata32(15)), rdata32(15, 0)),
            mode_LHU -> rdata32(15, 0).asUInt,
            mode_LW  -> rdata32(31, 0),
        ))
    }
  def mask_by_width(width: UInt) = {
    MuxLookup(width, 0.U, Seq( 
            0.U -> 1.U,
            1.U -> 3.U,
            2.U -> 7.U
        ))
  }
}

object csr_config {
    val CSR_SEPC        = 0x141.U
    val CSR_STVEC       = 0x105.U
    val CSR_SCAUSE      = 0x142.U
    val CSR_STVAL       = 0x143.U
    val CSR_SSCRATCH    = 0x140.U
    val CSR_SSTATUS     = 0x100.U
    val CSR_SATP        = 0x180.U
    val CSR_SIE         = 0x104.U
    val CSR_SIP         = 0x144.U
    val CSR_MTVEC       = 0x305.U
    val CSR_MEPC        = 0x341.U
    val CSR_MCAUSE      = 0x342.U
    val CSR_MIE         = 0x304.U
    val CSR_MIP         = 0x344.U
    val CSR_MTVAL       = 0x343.U
    val CSR_MSCRATCH    = 0x340.U
    val CSR_MSTATUS     = 0x300.U
    val CSR_MHARTID     = 0xf14.U
    val CSR_MEDELEG     = 0x302.U
    val CSR_MIDELEG     = 0x303.U
    val CSR_PMPADDR0    = 0x3b0.U
    val CSR_PMPADDR1    = 0x3b1.U
    val CSR_PMPADDR2    = 0x3b2.U
    val CSR_PMPADDR3    = 0x3b3.U
    val CSR_PMPCFG0     = 0x3a0.U
    val CSR_USCRATCH    = 0x40.U
    val CSR_MISA        = 0x301.U
    val CSR_SCOUNTEREN  = 0x106.U
    val CSR_MCOUNTEREN  = 0x306.U

    val PRV_U           = 0.U(2.W)
    val PRV_S           = 1.U(2.W)
    val PRV_M           = 3.U(2.W)
    val CAUSE_MISALIGNED_FETCH = 0x0
    val CAUSE_FETCH_ACCESS = 0x1
    val CAUSE_ILLEGAL_INSTRUCTION = 0x2
    val CAUSE_BREAKPOINT = 0x3
    val CAUSE_MISALIGNED_LOAD = 0x4
    val CAUSE_LOAD_ACCESS = 0x5
    val CAUSE_MISALIGNED_STORE = 0x6
    val CAUSE_STORE_ACCESS = 0x7
    val CAUSE_USER_ECALL = 0x8
    val CAUSE_SUPERVISOR_ECALL = 0x9
    val CAUSE_VIRTUAL_SUPERVISOR_ECALL = 0xa
    val CAUSE_MACHINE_ECALL = 0xb
    val CAUSE_FETCH_PAGE_FAULT = 0xc
    val CAUSE_LOAD_PAGE_FAULT = 0xd
    val CAUSE_STORE_PAGE_FAULT = 0xf
    val CAUSE_FETCH_GUEST_PAGE_FAULT = 0x14
    val CAUSE_LOAD_GUEST_PAGE_FAULT = 0x15
    val CAUSE_VIRTUAL_INSTRUCTION = 0x16
    val CAUSE_STORE_GUEST_PAGE_FAULT = 0x17

    val SSTATUS_UIE     = "h00000001".U(REG_WIDTH.W)
    val SSTATUS_SIE     = "h00000002".U(REG_WIDTH.W)
    val SSTATUS_UPIE    = "h00000010".U(REG_WIDTH.W)
    val SSTATUS_SPIE    = "h00000020".U(REG_WIDTH.W)
    val SSTATUS_UBE     = "h00000040".U(REG_WIDTH.W)
    val SSTATUS_SPP     = "h00000100".U(REG_WIDTH.W)
    val SSTATUS_VS      = "h00000600".U(REG_WIDTH.W)
    val SSTATUS_FS      = "h00006000".U(REG_WIDTH.W)
    val SSTATUS_XS      = "h00018000".U(REG_WIDTH.W)
    val SSTATUS_SUM     = "h00040000".U(REG_WIDTH.W)
    val SSTATUS_MXR     = "h00080000".U(REG_WIDTH.W)
    val SSTATUS32_SD    = "h80000000".U(REG_WIDTH.W)
    // val SSTATUS_UXL     = "h0000000300000000".U(REG_WIDTH.W)
    // val SSTATUS64_SD    = "h8000000000000000".U(REG_WIDTH.W)

    val MSTATUS_UIE     = "h00000001".U(REG_WIDTH.W)
    val MSTATUS_SIE     = "h00000002".U(REG_WIDTH.W)
    val MSTATUS_HIE     = "h00000004".U(REG_WIDTH.W)
    val MSTATUS_MIE     = "h00000008".U(REG_WIDTH.W)
    val MSTATUS_UPIE    = "h00000010".U(REG_WIDTH.W)
    val MSTATUS_SPIE    = "h00000020".U(REG_WIDTH.W)
    val MSTATUS_UBE     = "h00000040".U(REG_WIDTH.W)
    val MSTATUS_MPIE    = "h00000080".U(REG_WIDTH.W)
    val MSTATUS_SPP     = "h00000100".U(REG_WIDTH.W)
    val MSTATUS_VS      = "h00000600".U(REG_WIDTH.W)
    val MSTATUS_MPP     = "h00001800".U(REG_WIDTH.W)
    val MSTATUS_FS      = "h00006000".U(REG_WIDTH.W)
    val MSTATUS_XS      = "h00018000".U(REG_WIDTH.W)
    val MSTATUS_MPRV    = "h00020000".U(REG_WIDTH.W)
    val MSTATUS_SUM     = "h00040000".U(REG_WIDTH.W)
    val MSTATUS_MXR     = "h00080000".U(REG_WIDTH.W)
    val MSTATUS_TVM     = "h00100000".U(REG_WIDTH.W)
    val MSTATUS_TW      = "h00200000".U(REG_WIDTH.W)
    val MSTATUS_TSR     = "h00400000".U(REG_WIDTH.W)
    val MSTATUS32_SD    = "h80000000".U(REG_WIDTH.W)
    // val MSTATUS64_SD    = "h8000000000000000".U(REG_WIDTH.W)
    val MSTATUS_UIE_BIT     = 0
    val MSTATUS_SIE_BIT     = 1
    val MSTATUS_HIE_BIT     = 2
    val MSTATUS_MIE_BIT     = 3
    val MSTATUS_UPIE_BIT    = 4
    val MSTATUS_SPIE_BIT    = 5
    val MSTATUS_UBE_BIT     = 6
    val MSTATUS_MPIE_BIT    = 7
    val MSTATUS_SPP_BIT     = 8
    val MSTATUS_MPRV_BIT    = 17
    val MSTATUS_SUM_BIT     = 18
    val MSTATUS_MXR_BIT     = 19

    val IRQ_U_SOFT   = 0
    val IRQ_S_SOFT   = 1
    val IRQ_VS_SOFT  = 2
    val IRQ_M_SOFT   = 3
    val IRQ_U_TIMER  = 4
    val IRQ_S_TIMER  = 5
    val IRQ_VS_TIMER = 6
    val IRQ_M_TIMER  = 7
    val IRQ_U_EXT    = 8
    val IRQ_S_EXT    = 9
    val IRQ_VS_EXT   = 10
    val IRQ_M_EXT    = 11
    val IRQ_S_GEXT   = 12
    val IRQ_COP      = 12
    val IRQ_HOST     = 13
    val INVALID_IRQ  = 63

    val MIP_USIP     = (1 << IRQ_U_SOFT).U(REG_WIDTH.W)
    val MIP_SSIP     = (1 << IRQ_S_SOFT).U(REG_WIDTH.W)
    val MIP_VSSIP    = (1 << IRQ_VS_SOFT).U(REG_WIDTH.W)
    val MIP_MSIP     = (1 << IRQ_M_SOFT).U(REG_WIDTH.W)
    val MIP_UTIP     = (1 << IRQ_U_TIMER).U(REG_WIDTH.W)
    val MIP_STIP     = (1 << IRQ_S_TIMER).U(REG_WIDTH.W)
    val MIP_VSTIP    = (1 << IRQ_VS_TIMER).U(REG_WIDTH.W)
    val MIP_MTIP     = (1 << IRQ_M_TIMER).U(REG_WIDTH.W)
    val MIP_UEIP     = (1 << IRQ_U_EXT).U(REG_WIDTH.W)
    val MIP_SEIP     = (1 << IRQ_S_EXT).U(REG_WIDTH.W)
    val MIP_VSEIP    = (1 << IRQ_VS_EXT).U(REG_WIDTH.W)
    val MIP_MEIP     = (1 << IRQ_M_EXT).U(REG_WIDTH.W)
    val MIP_SGEIP    = (1 << IRQ_S_GEXT).U(REG_WIDTH.W)

    val ETYPE_NONE  = 0.U(2.W)
    val ETYPE_ECALL = 1.U(2.W)
    val ETYPE_SRET  = 2.U(2.W)
    val ETYPE_MRET  = 3.U(2.W)

    val MEDELEG_MASK = ((1 << CAUSE_MISALIGNED_FETCH) | (1 << CAUSE_BREAKPOINT) |
                    (1 << CAUSE_USER_ECALL) | (1 << CAUSE_SUPERVISOR_ECALL) |
                    (1 << CAUSE_FETCH_PAGE_FAULT) | (1 << CAUSE_LOAD_PAGE_FAULT) |
                    (1 << CAUSE_STORE_PAGE_FAULT)).U(REG_WIDTH.W)

    val SUP_INTS = MIP_SSIP | MIP_STIP | MIP_SEIP

    val RSSTATUS_MASK = SSTATUS_SIE | SSTATUS_SPIE | SSTATUS_SPP | SSTATUS_FS |
                        SSTATUS_XS | SSTATUS_SUM | SSTATUS_MXR //| SSTATUS64_SD | SSTATUS_UXL
    val WSSTATUS_MASK = SSTATUS_SIE | SSTATUS_SPIE | SSTATUS_SPP | SSTATUS_FS |
                        SSTATUS_XS | SSTATUS_SUM | SSTATUS_MXR
    val MSTATUS_MASK = MSTATUS_MIE | MSTATUS_MPIE | MSTATUS_MPRV | MSTATUS_SIE | MSTATUS_SPIE |
                        MSTATUS_TW | MSTATUS_TSR | MSTATUS_MXR | MSTATUS_SUM | MSTATUS_TVM |
                        MSTATUS_FS | MSTATUS_VS | MSTATUS_SPP | MSTATUS_MPP
    val PMPADDR_MASK = "hffffffff".U(REG_WIDTH.W)
    val W_SATP_MASK = "hffffffff".U(REG_WIDTH.W)

    val W_MIP_MASK = MIP_STIP | MIP_SSIP

    def set_partial_val(preVal: UInt, mask: UInt, newVal: UInt) = {
        (preVal & ~mask) | (newVal & mask)
    }

}

object decode_config extends DeType with ALUOP with BrType with Insts with MemMode{
    val IS_ALU64 = 0.U
    val IS_ALU32 = 1.U
    val NO_JMP     = "b00".U(2.W)
    val JMP_UNCOND = "b01".U(2.W)
    val JMP_COND   = "b10".U(2.W)
    val JMP_CSR    = "b11".U(2.W)

    val AMO_WIDTH = 5
    val amoSwap = "b00001".U(AMO_WIDTH.W)
    val amoAdd  = "b00000".U(AMO_WIDTH.W)
    val amoXor  = "b00100".U(AMO_WIDTH.W)
    val amoAnd  = "b01100".U(AMO_WIDTH.W)
    val amoOr   = "b01000".U(AMO_WIDTH.W)
    val amoMin  = "b10000".U(AMO_WIDTH.W)
    val amoMax  = "b10100".U(AMO_WIDTH.W)
    val amoMinU = "b11100".U(AMO_WIDTH.W)
    val amoMaxU = "b11100".U(AMO_WIDTH.W)

    val SPECIAL_FENCE_I = 1.U(2.W)
    val SPECIAL_SFENCE_VMA = 2.U(2.W)

    val INDI_WIDTH  = 2
    val INDI_LR_BIT = 0
    val INDI_SC_BIT = 1

    val SWAP_WIDTH  = 6
    val NO_SWAP     = "b011011".U(6.W)
    val SWAP_2_d    = "b011110".U(6.W)
    val COPY_2_d    = "b011010".U(6.W)

                            // decode aluop     ram-mode|write-reg|跳转信号
    val decodeDefault = List(INVALID, alu_NOP, mode_NOP, false.B, NO_JMP)
    val decodeTable = Array(   
        LUI    -> List(UType, alu_MV1,  mode_NOP, true.B,   NO_JMP),
        AUIPC  -> List(UType, alu_ADD,  mode_NOP, true.B,   NO_JMP),
        JAL    -> List(JType, alu_MV2,  mode_NOP, true.B,   JMP_UNCOND),
        JALR   -> List(IType, alu_MV2,  mode_NOP, true.B,   JMP_UNCOND),
        BEQ    -> List(BType, alu_NOP,  mode_NOP, false.B,  JMP_COND),
        BNE    -> List(BType, alu_NOP,  mode_NOP, false.B,  JMP_COND),
        BLT    -> List(BType, alu_NOP,  mode_NOP, false.B,  JMP_COND),
        BGE    -> List(BType, alu_NOP,  mode_NOP, false.B,  JMP_COND),
        BLTU   -> List(BType, alu_NOP,  mode_NOP, false.B,  JMP_COND),
        BGEU   -> List(BType, alu_NOP,  mode_NOP, false.B,  JMP_COND),

        LB     -> List(IType, alu_ADD,  mode_LB,  true.B,  NO_JMP),
        LH     -> List(IType, alu_ADD,  mode_LH,  true.B,  NO_JMP),
        LW     -> List(IType, alu_ADD,  mode_LW,  true.B,  NO_JMP),
        LBU    -> List(IType, alu_ADD,  mode_LBU, true.B,  NO_JMP),
        LHU    -> List(IType, alu_ADD,  mode_LHU, true.B,  NO_JMP),
        SB     -> List(SType, alu_ADD,  mode_SB,  false.B, NO_JMP),        
        SH     -> List(SType, alu_ADD,  mode_SH,  false.B, NO_JMP),
        SW     -> List(SType, alu_ADD,  mode_SW,  false.B, NO_JMP),
        ADDI   -> List(IType, alu_ADD,  mode_NOP, true.B,  NO_JMP),
        SLTI   -> List(IType, alu_SLT,  mode_NOP, true.B,  NO_JMP),
        SLTIU  -> List(IType, alu_SLTU, mode_NOP, true.B,  NO_JMP),
        XORI   -> List(IType, alu_XOR,  mode_NOP, true.B,  NO_JMP),
        ORI    -> List(IType, alu_OR,   mode_NOP, true.B,  NO_JMP),
        ANDI   -> List(IType, alu_AND,  mode_NOP, true.B,  NO_JMP),
        SLLI   -> List(IType, alu_SLL,  mode_NOP, true.B,  NO_JMP),
        SRLI   -> List(IType, alu_SRL,  mode_NOP, true.B,  NO_JMP),
        SRAI   -> List(IType, alu_SRA,  mode_NOP, true.B,  NO_JMP),

        ADD    -> List(RType, alu_ADD,  mode_NOP, true.B,  NO_JMP),
        SUB    -> List(RType, alu_SUB,  mode_NOP, true.B,  NO_JMP),
        SLL    -> List(RType, alu_SLL,  mode_NOP, true.B,  NO_JMP),
        SLT    -> List(RType, alu_SLT,  mode_NOP, true.B,  NO_JMP),
        SLTU   -> List(RType, alu_SLTU, mode_NOP, true.B,  NO_JMP),
        XOR    -> List(RType, alu_XOR,  mode_NOP, true.B,  NO_JMP),
        SRL    -> List(RType, alu_SRL,  mode_NOP, true.B,  NO_JMP),
        SRA    -> List(RType, alu_SRA,  mode_NOP, true.B,  NO_JMP),
        OR     -> List(RType, alu_OR,   mode_NOP, true.B,  NO_JMP),
        AND    -> List(RType, alu_AND,  mode_NOP, true.B,  NO_JMP),
                                                //|write-reg|跳转信号
        CSRRW  -> List(IType, alu_MV1,  mode_NOP, true.B,  NO_JMP),
        CSRRS  -> List(IType, alu_OR,   mode_NOP, true.B,  NO_JMP),
        CSRRC  -> List(IType, alu_NAND, mode_NOP, true.B,  NO_JMP),
        CSRRWI -> List(IType, alu_MV1,  mode_NOP, true.B,  NO_JMP),
        CSRRSI -> List(IType, alu_OR,   mode_NOP, true.B,  NO_JMP),
        CSRRCI -> List(IType, alu_NAND, mode_NOP, true.B,  NO_JMP),

        FENCE     -> List(EMPTY, alu_NOP, mode_NOP, false.B, NO_JMP),
        FENCE_I   -> List(EMPTY, alu_NOP, mode_NOP, false.B, NO_JMP),
        SFENCE_VMA-> List(EMPTY, alu_NOP, mode_NOP, false.B, NO_JMP),
        WFI       -> List(EMPTY, alu_NOP, mode_NOP, false.B, NO_JMP),
        TRAP      -> List(EMPTY, alu_NOP, mode_NOP, false.B, NO_JMP)
    )
}
