package io
import chisel3._
import chisel3.util._
import params.common._
import params.decode_config._

class AXIIO extends Bundle{
  val awready = Input(Bool())
  val awvalid = Output(Bool())
  val awaddr  = Output(UInt(32.W))
  val awid    = Output(UInt(4.W))
  val awlen   = Output(UInt(8.W))
  val awsize  = Output(UInt(3.W))
  val awburst = Output(UInt(2.W))
  val wready  = Input(Bool())
  val wvalid  = Output(Bool())
  val wdata   = Output(UInt(64.W))
  val wstrb   = Output(UInt(8.W))
  val wlast   = Output(Bool())
  val bready  = Output(Bool())
  val bvalid  = Input(Bool())
  val bresp   = Input(UInt(2.W))
  val bid     = Input(UInt(4.W))
  val arready = Input(Bool())
  val arvalid = Output(Bool())
  val araddr  = Output(UInt(32.W))
  val arid    = Output(UInt(4.W))
  val arlen   = Output(UInt(8.W))
  val arsize  = Output(UInt(3.W))
  val arburst = Output(UInt(2.W))
  val rready  = Output(Bool())
  val rvalid  = Input(Bool())
  val rresp   = Input(UInt(2.W))
  val rdata   = Input(UInt(64.W))
  val rlast   = Input(Bool())
  val rid     = Input(UInt(4.W))

  def init_i() = {
    awready := 0.U
    wready  := 0.U
    bvalid  := 0.U
    bresp   := 0.U
    bid     := 0.U
    arready := 0.U
    rvalid  := 0.U
    rresp   := 0.U
    rdata   := 0.U
    rlast   := 0.U
    rid     := 0.U
  }
  def init_o() = {
    awvalid := 0.U
    awaddr  := 0.U
    awid    := 0.U
    awlen   := 0.U
    awsize  := 0.U
    awburst := 0.U
    wvalid  := 0.U
    wdata   := 0.U
    wstrb   := 0.U
    wlast   := 0.U
    bready  := 0.U
    arvalid := 0.U
    araddr  := 0.U
    arid    := 0.U
    arlen   := 0.U
    arsize  := 0.U
    arburst := 0.U
    rready  := 0.U
  }
}

class CtrlEx extends Bundle {
  // val inst = Output(UInt(INST_WIDTH.W))
  // val pc = Output(UInt(REG_WIDTH.W))
  val aluOP = UInt(ALUOP_WIDTH.W)
  val wRegEn = Bool()
  val wCsrEn = Bool()
  val brType = UInt(3.W) //
}

class SimpleBus extends Bundle {
  val addr = Output(UInt(ADDR_WIDTH.W))
  val rdata = Input(UInt(REG_WIDTH.W))
  val wdata = Output(UInt(REG_WIDTH.W))
  val wmask = Output(UInt(4.W))
  val wen = Output(Bool())
  val valid = Output(Bool())
  val respValid = Input(Bool())
}

class IF2ID extends Bundle {
  val inst = Output(UInt(INST_WIDTH.W))
  val pc = Output(UInt(REG_WIDTH.W))
  val valid = Output(Bool())
  def init() = this := 0.U.asTypeOf(this)
}

class ID2EX extends Bundle {
  val inst = Output(UInt(INST_WIDTH.W))
  val ctrl = Output(new CtrlEx)
  val rs1_d = Output(UInt(REG_WIDTH.W))
  val rs2_d = Output(UInt(REG_WIDTH.W))
  val dst_id = Output(UInt(REG_NUM_WIDTH.W))
  val dst_d = Output(UInt(REG_WIDTH.W))
  val csr_id = Output(UInt(CSR_NUM_WIDTH.W))
  val valid = Output(Bool())
  val jmpType = Output(UInt(2.W))
  val pc = Output(UInt(REG_WIDTH.W))

  def init() = this := 0.U.asTypeOf(this)
}

class ID2MEM extends Bundle {
  val inst = Output(UInt(INST_WIDTH.W))
  val valid = Output(Bool())
  val memMode = Output(UInt(5.W))
  val addr = Output(UInt(ADDR_WIDTH.W))
  val data = Output(UInt(REG_WIDTH.W))
  val dst_id = Output(UInt(REG_NUM_WIDTH.W))
  val pc = Output(UInt(ADDR_WIDTH.W))
  def init() = this := 0.U.asTypeOf(this)
}

class EX2WB extends Bundle {
  val pc = Output(UInt(REG_WIDTH.W))
  val nextPC = Output(UInt(REG_WIDTH.W))
  val is_jmp = Output(Bool())
  val wreg = Flipped(new WriteReg)
  val wcsr = Flipped(new WriteCSR)
  val valid = Output(Bool())

  def init() = this := 0.U.asTypeOf(this)
}

class MEM2WB extends Bundle {
  val pc = Output(UInt(REG_WIDTH.W))
  val nextPC = Output(UInt(REG_WIDTH.W))
  val wreg = Flipped(new WriteReg)
  val valid = Output(Bool())
  def init() = this := 0.U.asTypeOf(this)
}

class ReadCSR extends Bundle {
  val id = Input(UInt(CSR_NUM_WIDTH.W))
  val data = Output(UInt(REG_WIDTH.W))
}

class WriteCSR extends Bundle {
  val id = Input(UInt(CSR_NUM_WIDTH.W))
  val data = Input(UInt(REG_WIDTH.W))
  val en = Input(Bool())
}

class ReadReg extends Bundle {
  val id = Input(UInt(REG_WIDTH.W))
  val data = Output(UInt(REG_WIDTH.W))
}

class WriteReg extends Bundle {
  val id = Input(UInt(REG_WIDTH.W))
  val data = Input(UInt(REG_WIDTH.W))
  val en = Input(Bool())
}
