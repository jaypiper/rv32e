package cpu
import chisel3._
import chisel3.util._
import alu._

class Regs extends Module {
  val io = IO(new Bundle{
    val rs1 = new ReadReg
    val rs2 = new ReadReg
    val dst = new WriteReg
  })
  val regs = RegInit(VecInit(Seq.fill(REG_NUM)(0.U(REG_WIDTH.W))))
  io.rs1.data := regs(io.rs1.id)
  io.rs2.data := regs(io.rs2.id)
  when(io.dst.em\n && io.dst.id =/= 0.U) {
    regs(io.dst.id) := io.dst.data
  }
}

class Csrs extends Module {
  val io = IO(new Bundle {
    val rs      = new ReadCSR
    val rd      = new WriteCSR
  })

}


class Fetch extends Module {
  val io = IO(new Bundle {
    val nextPC = Input(UInt(REG_WIDTH.W))
    val instFinish = Input(Bool())
    val simpleBus = new SimpleBus
    val if2id = new IF2ID
  })
  val pc = RegInit(PC_START)
  io.simpleBus.addr = pc
  io.simpleBus.wen = false.B
  when (instFinish) {
    pc := nextPC
  }
  io.if2id.inst := io.simpleBus.rdata
  io.if2id.pc := pc
}

class Decode extends Module {
  val io = IO(new Bundle {
    val if2id = Flipped(new IF2ID)
    val id2ex = new ID2EX
    val id2mem = new ID2MEM
    val rrs1 = Flipped(new ReadReg)
    val rrs2 = Flipped(new ReadReg)
    val rcsr = Flipped(new ReadCSR)
  })

  val instType = ListLookup(io.if2id.inst, decodeDefault, decodeTable)
  val dType = instType(0)
  val jmp_indi = instType(5) === true.B
  val is_csr = io.if2id.inst(6,0) == "b1110011"
  val rs1_imm = is_csr && io.if2id.inst(14)
  val imm = Wire(SInt(REG_WIDTH.W))

  imm := 0.S
  switch(dType){
      is(IType){ imm := inst_in(31,20).asSInt }
      is(SType){ imm := Cat(inst_in(31, 25), inst_in(11, 7)).asSInt }
      is(BType){ imm := Cat(inst_in(31), inst_in(7), inst_in(30, 25), inst_in(11, 8), 0.U(1.W)).asSInt }
      is(UType){ imm := Cat(inst_in(31, 12), 0.U(12.W)).asSInt }
      is(JType){ imm := Cat(inst_in(31), inst_in(19, 12), inst_in(20), inst_in(30, 21), 0.U(1.W)).asSInt }
  }
  io.rrs1.id := io.if2id.inst(19, 15)
  io.rrs2.id := io.if2id.inst(24, 20)
  io.rcsr.id := io.if2id.inst(31, 20)

  io.id2ex.inst := io.if2id.inst
  io.id2ex.ctrl.aluOP := instType(1)
  io.id2ex.ctrl.writeRegEn := instType(3) && (instType(2) === mode_NOP)
  io.id2ex.ctrl.writeCSREn := is_csr
  io.id2ex.ctrl.brType := io.if2id.inst(14,12)
  io.id2ex.rs1_d := rs1_imm ? Cat(Fill(REG_WIDTH-5, io.rrs1.id(4), io.rrs1.id)) : io.rrs1.data
  io.id2ex.rs2_d := is_csr ? io.rcsr.data : io.rrs2.data
  io.id2ex.dst_id := io.if2id.inst(11, 7)
  io.id2ex.dst_d := io.if2id.pc + imm
  io.id2ex.csr_id := io.if2id.inst(31, 20)
  io.id2ex.valid := instType(2) =/= mode_NOP
  io.id2ex.jmpType := instType(4)

  io.id2mem.inst := io.if2id.inst
  io.id2mem.valid := instType(2) === mode_NOP
  io.id2mem.ctrl.memMode := instType(2)
  io.id2mem.addr := io.rrs1.data + imm
  io.id2mem.data := io.rrs2.data
  io.id2mem.dst_id := io.if2id.inst(11, 7)
   
}

class Execute extends Module {
  val io = IO(new Bundle {
    val id2ex = Flipped(new ID2EX)
    val ex2wb = new EX2WB
  })

  val alu = new ALU
  alu.io.alu_op := io.id2ex.ctrl.aluOP
  alu.io.val1 := io.id2ex.rs1_d
  alu.io.val2 := io.id2ex.rs2_d
  
  val balu = new BranchALU
  balu.io.val1 := io.id2ex.rs1_d
  balu.io.val2 := io.id2ex.rs2_d
  balu.io.brType := io.id2ex.ctrl.brType

  val is_jmp = io.id2ex.jmpType === JMP_UNCOND || (io.id2ex.jmpType === JMP_COND && balu.io.is_jmp)
  val nextPc = is_jmp ? io.id2ex.dst_d : io.id2ex.pc + 4.U
  io.ex2wb.pc := pc
  io.ex2wb.nextPC := nextPC
  io.ex2wb.is_jmp := is_jmp
  io.ex2wb.wreg.id := io.id2ex.dst_id
  io.ex2wb.wreg.data := io.id2ex.ctrl.wCsrEn : io.id2ex.rs2_d : alu.io.out
  io.ex2wb.wreg.en := io.id2ex.ctrl.wRegEn
  io.ex2wb.wcsr.id := io.id2ex.csr_id
  io.ex2wb.wcsr.data := alu.io.out
  io.ex2wb.wcsr.en := io.id2ex.ctrl.wRegEn
  io.ex2wb.valid := io.id2ex.valid
}

class Memory extends Module {
  val io = IO(new Bundle {
    val id2mem = Flipped(new ID2MEM)
    val memIO = new SimpleBus
    val mem2wb = new MEM2WB
  })

  val sIdle :: sWaitMem :: Nil = Enum(2)
  val state = RegINit(sIdle)
  val id_r = RegInit(0.U(REG_NUM_WIDTH.W))
  val valid_r = RegInit(false.B)
  val mode_r = RegInit(0.U(5.W))
  val addr_r = RegInit(0.U(ADDR_WIDTH.W))
  val pc_r = RegInit(0.U(ADDR_WIDTH.W))
  switch(state) {
    is(sIdle) {
      when(io.memIO.valid && !io.memIO.respValid) {
        state := sWaitMem
        id_r := io.id2mem.dst_id
        mode_r := io.id2mem.memMode
        addr_r = io.id2mem.addr
        valid_r := true.B
        pc_r := io.id2mem.pc
      }
    }
    is(sWaitMem) {
      when(io.memIO.respValid) {
        state := sIdle
        valid_r := false.B
      }
    }
  }

  val curMode = (state === sIdle) ? io.id2mem.memMode : mode_r
  val curAddr = (state === sIdle) ? io.id2mem.addr : addr_r
  val memFinish = ((io.id2ex.valid && (state === sIdle)) || (state === sWaitMem)) && io.memIO.respValid

  io.memIO.addr := io.id2men.addr & 0xfffffffc
  io.memIO.wdata := io.id2mem.data
  io.memIO.valid := io.id2mem.valid
  io.memIO.wen :=  io.id2mem.memMode(3)
  io.memIO.wmask := mask_by_width(io.id2mem.memMode(1,0)) << io.memIO.addr(1, 0)

  io.mem2wb.valid := memFinish
  io.mem2wb.wreg.id := (state === sIdle) ? io.id2mem.dst_id : id_r
  io.mem2wb.wreg.data := rdata_by_mode(curMode, io.memIO.rdata >> Cat(curAddr(1,0), 0.U(3.W)))
  io.mem2wb.wreg.en := (state === sIdle) ? io.id2mem.memMode(2) && mode_r(2)
  io.mem2wb.pc := state === sIdle ? io.id2mem.pc : pc_r
  io.mem2wb.nextPC := io.mem2wb.pc + 4.U
 }

class WriteBack extends Module {
  val io = IO(new Bundle {
    val ex2wb = Flipped(new EX2WB)
    val mem2wb = Flipped(new EX2RB)
    val instFinish = Output(Bool())
    val wreg = Flipped(new WriteReg)
    val wcsr = Flipped(new WriteReg)
    val nextPC = Output(UInt(ADDR_WIDTH.W))
  })

  io.instFinish := io.ex2wb.valid || io.mem2wb.valid
  io.wreg := io.ex2wb.valid ? io.ex2wb.wreg : (io.mem2wb.valid ? io.mem2wb.wreg : 0.U)
  io.wcsr := io.ex2wb.valid ? io.ex2wb.wcsr : 0.U
  io.nextPC := io.ex2wb.valid ? io.ex2wb.nextPC : io.mem2wb.nextPC
}

class rv32e extends Module {
  val io = IO(new Bundle {
    val master = new SimpleBus
  })
  dontTouch(io)

  val fetch = Module(new Fetch)
  val decode = Module(new Decode)
  val execute = Module(new Execute)
  val memory = Module(new Memory)
  val writeback = Module(new WriteBack)
  val reg = Module(new Regs)
  
  fetch.io.nextPC <> writeback.io.nextPC
  fetch.io.instFinish <> writeback.io.instFinish
  fetch.io.simpleBus <> io.master
  fetch.io.if2id <> decode.io.if2id

  decode.io.id2ex <> execute.io.id2ex
  decode.io.id2mem <> memory.io.id2mem
  decode.io.rrs1 <> regs.io.rs1
  decode.io.rrs2 <> regs.io.rs2
  // decode.io.rcsr <>

  execute.io.ex2wb <> writeback.io.ex2wb

  memory.io.mem2wb <> writeback.io.mem2wb
  
  writeback.io.wreg <> regs.dst
}

