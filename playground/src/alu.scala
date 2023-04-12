package alu
import chisel3._
import chisel3.util._
import io._
import params._

class ALUIO extends Bundle{
    val alu_op  = Input(UInt(ALUOP_WIDTH.W))
    val val1    = Input(UInt(REG_WIDTH.W))
    val val2    = Input(UInt(REG_WIDTH.W))
    val out     = Output(UInt(REG_WIDTH.W))
}

class ALU extends Module{
    val io = IO(new ALUIO)
    val alu_val = MuxLookup(io.alu_op, 0.U(DATA_WIDTH.W), Seq(
        alu_NOP     -> (0.U(DATA_WIDTH.W)),
        alu_MV1     -> (io.val1),
        alu_MV2     -> (io.val2),
        alu_ADD     -> (io.val1 + io.val2), 
        alu_XOR     -> (io.val1 ^ io.val2),
        alu_OR      -> (io.val1 | io.val2),
        alu_AND     -> (io.val1 & io.val2),
        alu_SLL     -> (io.val1 << io.val2(4,0)),
        alu_SRL     -> (io.val1.asUInt >> io.val2(4,0)),
        alu_SRA     -> ((io.val1.asSInt >> io.val2(4,0)).asUInt),
        alu_SUB     -> (io.val1 - io.val2),
        alu_SLT     -> Mux(io.val1.asSInt < io.val2.asSInt, 1.U, 0.U),
        alu_SLTU    -> Mux(io.val1 < io.val2, 1.U, 0.U),
        alu_NAND    -> ((~io.val1) & io.val2)
    ))
    io.out := alu_val
  
}

class BranchALUIO extends Bundle{
    val val1    = Input(UInt(REG_WIDTH.W))
    val val2    = Input(UInt(REG_WIDTH.W))
    val brType  = Input(UInt(3.W))
    val is_jmp  = Output(Bool())
}

class BranchALU extends Module{
    val io = IO(new BranchALUIO)
    io.is_jmp := MuxLookup(io.brType, false.B, Seq(
        bEQ     -> (io.val1 === io.val2),
        bNE     -> (io.val1 =/= io.val2),
        bLT     -> (io.val1.asSInt < io.val2.asSInt),
        bGE     -> (io.val1.asSInt >= io.val2.asSInt),
        bLTU    -> (io.val1 < io.val2),
        bGEU    -> (io.val1 >= io.val2)
    ))
}