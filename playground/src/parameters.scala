package params

import chisel3._
import chisel3.util._

object common {
  val REG_WIDTH = 32
  val REG_NUM = 16
  val REG_NUM_WIDTH = Log2Up(REG_NUM)
  val ADDR_WIDTH = 32
  val INST_WIDTH = 32
  val CSR_WIDTH = 32
  val CSR_WIDTH = 12
  val PC_START = "h30000000".U(ADDR_WIDTH.W)
}

object Insts{
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
            mode_LB  -> Cat(Fill(DATA_WIDTH - 8, rdata32(7)), rdata32(7, 0)),
            mode_LBU -> rdata32(7, 0).asUInt,
            mode_LH  -> Cat(Fill(DATA_WIDTH - 16, rdata32(15)), rdata32(15, 0)),
            mode_LHU -> rdata32(15, 0).asUInt,
            mode_LW  -> Cat(Fill(DATA_WIDTH - 32, rdata32(31)), rdata32(31, 0)),
        ))
    }
  def mask_by_width(width: UInt) = {
    MuxLookup(width, 0.U, Seq( 
            0.U -> 1.U
            1.U -> 3.U
            2.U -> 7.U
        ))
  }
}

object decode_config extends DeType with ALUOP with BrType 
          with csr_config with dataForw{
    val IS_ALU64 = 0.U
    val IS_ALU32 = 1.U
                            // decode aluop     ram-mode|write-reg|跳转信号
    val decodeDefault = List(INVALID, alu_NOP, mode_NOP, false.B, NO_JMP)
    val decodeTable = Array(   
        Insts.LUI    -> List(UType, alu_MV1,  mode_NOP, true.B,   NO_JMP),
        Insts.AUIPC  -> List(UType, alu_ADD,  mode_NOP, true.B,   NO_JMP),
        Insts.JAL    -> List(JType, alu_MV2,  mode_NOP, true.B,   JMP_UNCOND),
        Insts.JALR   -> List(IType, alu_MV2,  mode_NOP, true.B,   JMP_UNCOND),
        Insts.BEQ    -> List(BType, alu_NOP,  mode_NOP, false.B,  JMP_COND),
        Insts.BNE    -> List(BType, alu_NOP,  mode_NOP, false.B,  JMP_COND),
        Insts.BLT    -> List(BType, alu_NOP,  mode_NOP, false.B,  JMP_COND),
        Insts.BGE    -> List(BType, alu_NOP,  mode_NOP, false.B,  JMP_COND),
        Insts.BLTU   -> List(BType, alu_NOP,  mode_NOP, false.B,  JMP_COND),
        Insts.BGEU   -> List(BType, alu_NOP,  mode_NOP, false.B,  JMP_COND),

        Insts.LB     -> List(IType, alu_ADD,  mode_LB,  true.B,  NO_JMP),
        Insts.LH     -> List(IType, alu_ADD,  mode_LH,  true.B,  NO_JMP),
        Insts.LW     -> List(IType, alu_ADD,  mode_LW,  true.B,  NO_JMP),
        Insts.LBU    -> List(IType, alu_ADD,  mode_LBU, true.B,  NO_JMP),
        Insts.LHU    -> List(IType, alu_ADD,  mode_LHU, true.B,  NO_JMP),
        Insts.SB     -> List(SType, alu_ADD,  mode_SB,  false.B, NO_JMP),        
        Insts.SH     -> List(SType, alu_ADD,  mode_SH,  false.B, NO_JMP),
        Insts.SW     -> List(SType, alu_ADD,  mode_SW,  false.B, NO_JMP),
        Insts.ADDI   -> List(IType, alu_ADD,  mode_NOP, true.B,  NO_JMP),
        Insts.SLTI   -> List(IType, alu_SLT,  mode_NOP, true.B,  NO_JMP),
        Insts.SLTIU  -> List(IType, alu_SLTU, mode_NOP, true.B,  NO_JMP),
        Insts.XORI   -> List(IType, alu_XOR,  mode_NOP, true.B,  NO_JMP),
        Insts.ORI    -> List(IType, alu_OR,   mode_NOP, true.B,  NO_JMP),
        Insts.ANDI   -> List(IType, alu_AND,  mode_NOP, true.B,  NO_JMP),
        Insts.SLLI   -> List(IType, alu_SLL,  mode_NOP, true.B,  NO_JMP),
        Insts.SRLI   -> List(IType, alu_SRL,  mode_NOP, true.B,  NO_JMP),
        Insts.SRAI   -> List(IType, alu_SRA,  mode_NOP, true.B,  NO_JMP),

        Insts.ADD    -> List(RType, alu_ADD,  mode_NOP, true.B,  NO_JMP),
        Insts.SUB    -> List(RType, alu_SUB,  mode_NOP, true.B,  NO_JMP),
        Insts.SLL    -> List(RType, alu_SLL,  mode_NOP, true.B,  NO_JMP),
        Insts.SLT    -> List(RType, alu_SLT,  mode_NOP, true.B,  NO_JMP),
        Insts.SLTU   -> List(RType, alu_SLTU, mode_NOP, true.B,  NO_JMP),
        Insts.XOR    -> List(RType, alu_XOR,  mode_NOP, true.B,  NO_JMP),
        Insts.SRL    -> List(RType, alu_SRL,  mode_NOP, true.B,  NO_JMP),
        Insts.SRA    -> List(RType, alu_SRA,  mode_NOP, true.B,  NO_JMP),
        Insts.OR     -> List(RType, alu_OR,   mode_NOP, true.B,  NO_JMP),
        Insts.AND    -> List(RType, alu_AND,  mode_NOP, true.B,  NO_JMP),
                                                      //|write-reg|跳转信号
        Insts.CSRRW  -> List(IType, alu_MV1,  mode_NOP, true.B,  NO_JMP),
        Insts.CSRRS  -> List(IType, alu_OR,   mode_NOP, true.B,  NO_JMP),
        Insts.CSRRC  -> List(IType, alu_NAND, mode_NOP, true.B,  NO_JMP),
        Insts.CSRRWI -> List(IType, alu_MV1,  mode_NOP, true.B,  NO_JMP),
        Insts.CSRRSI -> List(IType, alu_OR,   mode_NOP, true.B,  NO_JMP),
        Insts.CSRRCI -> List(IType, alu_NAND, mode_NOP, true.B,  NO_JMP),

        Insts.FENCE     -> List(EMPTY, alu_NOP, mode_NOP, false.B, NO_JMP),
        Insts.FENCE_I   -> List(EMPTY, alu_NOP, mode_NOP, false.B, NO_JMP),
        Insts.SFENCE_VMA-> List(EMPTY, alu_NOP, mode_NOP, false.B, NO_JMP),
        Insts.WFI       -> List(EMPTY, alu_NOP, mode_NOP, false.B, NO_JMP),
        Insts.TRAP      -> List(EMPTY, alu_NOP, mode_NOP, false.B, NO_JMP)
    )

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
}