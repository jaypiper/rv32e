package cpu
import chisel3._
import chisel3.util._
import alu._
import io._
import params.common._
import params.decode_config._
import params.csr_config._
import params.axi_config._

class UpdateRegs extends BlackBox with HasBlackBoxPath{
  val io = IO(new Bundle{
      val regs_data   = Input(UInt((32*REG_WIDTH).W))
      val clock       = Input(Clock())

  })
  addPath("playground/src/UpdateRegs.v")
}

class UpdateInst extends BlackBox with HasBlackBoxPath{
    val io = IO(new Bundle{
        val clock   = Input(Clock())
        val valid   = Input(Bool())
        val pc      = Input(UInt(ADDR_WIDTH.W))
        val inst    = Input(UInt(INST_WIDTH.W))
        val isMMIO  = Input(Bool())
    })
    addPath("playground/src/UpdateInst.v")
}

class Regs extends Module {
  val io = IO(new Bundle{
    val rs1 = new ReadReg
    val rs2 = new ReadReg
    val dst = new WriteReg
  })
  val regs = RegInit(VecInit(Seq.fill(REG_NUM)(0.U(REG_WIDTH.W))))
  io.rs1.data := regs(io.rs1.id)
  io.rs2.data := regs(io.rs2.id)
  when(io.dst.en && io.dst.id =/= 0.U) {
    regs(io.dst.id) := io.dst.data
  }
  val updateRegs = Module(new UpdateRegs)
  updateRegs.io.regs_data := regs.asUInt
  updateRegs.io.clock := clock
}

class Csrs extends Module {
  val io = IO(new Bundle {
    val rs      = new ReadCSR
    val rd      = new WriteCSR
  })
// FIX ME!!!
  val priv        = RegInit(PRV_M)
  val misa        = RegInit("h0014112d".U(REG_WIDTH.W))
  val mstatus     = RegInit("ha0000000".U(REG_WIDTH.W))
  val mepc        = RegInit(0.U(REG_WIDTH.W))
  val mtval       = RegInit(0.U(REG_WIDTH.W))
  val mscratch    = RegInit(0.U(REG_WIDTH.W))
  val mcause      = RegInit(0.U(REG_WIDTH.W))
  val mtvec       = RegInit(0.U(REG_WIDTH.W))
  val mie         = RegInit(0.U(REG_WIDTH.W))
  val mip         = RegInit(0.U(REG_WIDTH.W))
  val medeleg     = RegInit(0.U(REG_WIDTH.W))
  val mideleg     = RegInit(0.U(REG_WIDTH.W))
  val mcounteren  = RegInit(0.U(32.W))
  val scounteren  = RegInit(0.U(32.W))
  val sepc        = RegInit(0.U(REG_WIDTH.W))
  val stval       = RegInit(0.U(REG_WIDTH.W))
  val sscratch    = RegInit(0.U(REG_WIDTH.W))
  val stvec       = RegInit(0.U(REG_WIDTH.W))
  val satp        = RegInit(0.U(REG_WIDTH.W))
  val scause      = RegInit(0.U(REG_WIDTH.W))
  val pmpaddr0    = RegInit(0.U(REG_WIDTH.W))
  val pmpaddr1    = RegInit(0.U(REG_WIDTH.W))
  val pmpaddr2    = RegInit(0.U(REG_WIDTH.W))
  val pmpaddr3    = RegInit(0.U(REG_WIDTH.W))
  val uscratch    = RegInit(0.U(REG_WIDTH.W))
  val pmpcfg0     = RegInit(0.U(REG_WIDTH.W))
  val mhartid     = RegInit(0.U(REG_WIDTH.W))
  val sstatus     = mstatus & RSSTATUS_MASK
// csr read
  when(io.rs.id === CSR_MISA){
      io.rs.data := misa
  }.elsewhen(io.rs.id === CSR_MSTATUS){
      io.rs.data := mstatus
  }.elsewhen(io.rs.id === CSR_MEPC){
      io.rs.data := mepc
  }.elsewhen(io.rs.id === CSR_MTVAL){
      io.rs.data := mtval
  }.elsewhen(io.rs.id === CSR_MSCRATCH){
      io.rs.data := mscratch
  }.elsewhen(io.rs.id === CSR_MTVEC){
      io.rs.data := mtvec
  }.elsewhen(io.rs.id === CSR_MIE){
      io.rs.data := mie
  }.elsewhen(io.rs.id === CSR_MIP){
      io.rs.data := mip
  }.elsewhen(io.rs.id === CSR_MCAUSE){
      io.rs.data := mcause
  }.elsewhen(io.rs.id === CSR_MEDELEG){
      io.rs.data := medeleg
  }.elsewhen(io.rs.id === CSR_MIDELEG){
      io.rs.data := mideleg
  }.elsewhen(io.rs.id === CSR_MCOUNTEREN){
      io.rs.data := mcounteren
  }.elsewhen(io.rs.id === CSR_SCOUNTEREN){
      io.rs.data := scounteren
  }.elsewhen(io.rs.id === CSR_SEPC){
      io.rs.data := sepc
  }.elsewhen(io.rs.id === CSR_STVAL){
      io.rs.data := stval
  }.elsewhen(io.rs.id === CSR_SSCRATCH){
      io.rs.data := sscratch
  }.elsewhen(io.rs.id === CSR_STVEC){
      io.rs.data := stvec
  }.elsewhen(io.rs.id === CSR_SATP){
      io.rs.data := satp
  }.elsewhen(io.rs.id === CSR_SCAUSE){
      io.rs.data := scause
  }.elsewhen(io.rs.id === CSR_SSTATUS){
      io.rs.data := sstatus
  }.elsewhen(io.rs.id === CSR_SIE){
      io.rs.data := mie & mideleg
  }.elsewhen(io.rs.id === CSR_SIP){
      io.rs.data := mip & SUP_INTS
  }.elsewhen(io.rs.id === CSR_PMPADDR0){
      io.rs.data := pmpaddr0
  }.elsewhen(io.rs.id === CSR_PMPADDR1){
      io.rs.data := pmpaddr1
  }.elsewhen(io.rs.id === CSR_PMPADDR2){
      io.rs.data := pmpaddr2
  }.elsewhen(io.rs.id === CSR_PMPADDR3){
      io.rs.data := pmpaddr3
  }.elsewhen(io.rs.id === CSR_PMPCFG0){
      io.rs.data := pmpaddr3
  }.elsewhen(io.rs.id === CSR_USCRATCH){
      io.rs.data := uscratch
  }.elsewhen(io.rs.id === CSR_MHARTID){
      io.rs.data := mhartid
  }.otherwise{
      io.rs.data      := "habcdabcd".U
  }
  when(!io.rd.en){
  }.elsewhen(io.rd.id === CSR_MISA){
      misa :=  io.rd.data
  }.elsewhen(io.rd.id === CSR_MSTATUS){
      val new_mstatus = io.rd.data & MSTATUS_MASK
      val sd          = Mux((io.rd.data(14,13) === 3.U) || (io.rd.data(16,15) === 3.U), MSTATUS32_SD, 0.U)
      mstatus := set_partial_val(mstatus, MSTATUS_MASK | MSTATUS32_SD, new_mstatus | sd)
  }.elsewhen(io.rd.id === CSR_MEPC){
      mepc := io.rd.data
  }.elsewhen(io.rd.id === CSR_MTVAL){
      mtval := io.rd.data
  }.elsewhen(io.rd.id === CSR_MSCRATCH){
      mscratch := io.rd.data
  }.elsewhen(io.rd.id === CSR_MTVEC){
      mtvec := io.rd.data
  }.elsewhen(io.rd.id === CSR_MIE){
      mie := io.rd.data
  }.elsewhen(io.rd.id === CSR_MIP){
      mip := set_partial_val(mip, W_MIP_MASK, io.rd.data)
  }.elsewhen(io.rd.id === CSR_MCAUSE){
      mcause := io.rd.data
  }.elsewhen(io.rd.id === CSR_MEDELEG){
      medeleg := io.rd.data & MEDELEG_MASK
  }.elsewhen(io.rd.id === CSR_MIDELEG){
      mideleg := io.rd.data & SUP_INTS
  }.elsewhen(io.rd.id === CSR_MCOUNTEREN){
      mcounteren := io.rd.data
  }.elsewhen(io.rd.id === CSR_SCOUNTEREN){
      scounteren := io.rd.data
  }.elsewhen(io.rd.id === CSR_SEPC){
      sepc := io.rd.data
  }.elsewhen(io.rd.id === CSR_STVAL){
      stval := io.rd.data
  }.elsewhen(io.rd.id === CSR_SSCRATCH){
      sscratch := io.rd.data
  }.elsewhen(io.rd.id === CSR_STVEC){
      stvec := io.rd.data
  }.elsewhen(io.rd.id === CSR_SATP){
      satp := set_partial_val(satp, W_SATP_MASK, io.rd.data)
  }.elsewhen(io.rd.id === CSR_SCAUSE){
      scause := io.rd.data
  }.elsewhen(io.rd.id === CSR_SSTATUS){
      val new_mstatus = set_partial_val(mstatus, WSSTATUS_MASK, io.rd.data)
      val sd          = Mux((io.rd.data(14,13) === 3.U) || (io.rd.data(16,15) === 3.U), MSTATUS32_SD, 0.U)
      // mstatus := Cat(sd(63,62), new_mstatus(61,0))
  }.elsewhen(io.rd.id === CSR_SIE){
      mie := set_partial_val(mie, mideleg, io.rd.data)
  }.elsewhen(io.rd.id === CSR_SIP){
      mip := set_partial_val(mip, SUP_INTS, io.rd.data)
  }.elsewhen(io.rd.id === CSR_PMPADDR0){
      pmpaddr0 := io.rd.data & PMPADDR_MASK
  }.elsewhen(io.rd.id === CSR_PMPADDR1){
      pmpaddr1 := io.rd.data & PMPADDR_MASK
  }.elsewhen(io.rd.id === CSR_PMPADDR2){
      pmpaddr2 := io.rd.data & PMPADDR_MASK
  }.elsewhen(io.rd.id === CSR_PMPADDR3){
      pmpaddr3 := io.rd.data & PMPADDR_MASK
  }.elsewhen(io.rd.id === CSR_PMPCFG0){
      pmpcfg0 := io.rd.data
  }.elsewhen(io.rd.id === CSR_USCRATCH){
      uscratch := io.rd.data
  }.elsewhen(io.rd.id === CSR_MHARTID){
      mhartid := io.rd.data
  }.otherwise{

  }
}

class Fetch extends Module {
  val io = IO(new Bundle {
    val nextPC = Input(UInt(REG_WIDTH.W))
    val instFinish = Input(Bool())
    val simpleBus = new SimpleBus
    val if2id = new IF2ID
  })
  val pc = RegInit(PC_START)
  val sIdle :: sWait :: Nil = Enum(2)
  val state = RegInit(sIdle)
  val prevFinish = RegInit(true.B)
	val prevPc = RegInit(0.U(ADDR_WIDTH.W))
	val hs_r = RegInit(false.B)
  when(io.instFinish) {
    prevFinish := true.B
  } .elsewhen(io.if2id.valid) {
    prevFinish := false.B
  }
	when(io.simpleBus.valid & io.simpleBus.ready) {
		hs_r := true.B
		prevPc := pc
	}.elsewhen(io.simpleBus.respValid) {
		hs_r := false.B
	}
  io.simpleBus.addr := pc & (~0x7.U(ADDR_WIDTH.W))
  io.simpleBus.wdata := 0.U
  io.simpleBus.wmask := 0.U
  io.simpleBus.wen := false.B
  io.simpleBus.valid := prevFinish && !hs_r
	io.simpleBus.wsize := 0.U

  when (io.instFinish) {
    pc := io.nextPC
  }

  io.if2id.inst := Mux(prevPc(2), io.simpleBus.rdata(63, 32), io.simpleBus.rdata(31, 0))
  io.if2id.pc := pc
  io.if2id.valid := io.simpleBus.respValid
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
  val is_csr = io.if2id.inst(6,0) === "b1110011".U
  val rs1_imm = is_csr && io.if2id.inst(14)
  val imm = Wire(SInt(REG_WIDTH.W))

  imm := 0.S
  switch(dType){
      is(IType){ imm := io.if2id.inst(31,20).asSInt }
      is(SType){ imm := Cat(io.if2id.inst(31, 25), io.if2id.inst(11, 7)).asSInt }
      is(BType){ imm := Cat(io.if2id.inst(31), io.if2id.inst(7), io.if2id.inst(30, 25), io.if2id.inst(11, 8), 0.U(1.W)).asSInt }
      is(UType){ imm := Cat(io.if2id.inst(31, 12), 0.U(12.W)).asSInt }
      is(JType){ imm := Cat(io.if2id.inst(31), io.if2id.inst(19, 12), io.if2id.inst(20), io.if2id.inst(30, 21), 0.U(1.W)).asSInt }
  }
  io.rrs1.id := io.if2id.inst(19, 15)
  io.rrs2.id := io.if2id.inst(24, 20)
  io.rcsr.id := io.if2id.inst(31, 20)

  io.id2ex.inst := io.if2id.inst
  io.id2ex.ctrl.aluOP := instType(1)
  io.id2ex.ctrl.wRegEn := (instType(3) === true.B) && (instType(2) === mode_NOP)
  io.id2ex.ctrl.wCsrEn := is_csr
  io.id2ex.ctrl.brType := io.if2id.inst(14,12)
  io.id2ex.rs1_d := PriorityMux(Seq(
    (rs1_imm, Cat(Fill(REG_WIDTH-5, io.rrs1.id(4)), io.rrs1.id)),
    (dType === UType, io.if2id.pc),
    (dType === JType, io.if2id.pc + 4.U),
    (true.B, io.rrs1.data)))
  io.id2ex.rs2_d := PriorityMux(Seq(
    (is_csr, io.rcsr.data),
    (dType === IType && instType(4) === JMP_UNCOND, io.if2id.pc + 4.U),
    (dType === UType || dType === IType, imm.asUInt),
    (true.B, io.rrs2.data)))
  io.id2ex.dst_id := io.if2id.inst(11, 7)
  io.id2ex.dst_d := PriorityMux(Seq(
    (dType === IType, (io.rrs1.data.asSInt + imm).asUInt & (~1.U(REG_WIDTH.W))),
    (true.B, (io.if2id.pc.asSInt + imm).asUInt)))
  io.id2ex.csr_id := io.if2id.inst(31, 20)
  io.id2ex.valid := io.if2id.valid && (instType(2) === mode_NOP)
  io.id2ex.jmpType := instType(4)
  io.id2ex.pc := io.if2id.pc

  io.id2mem.inst := io.if2id.inst
  io.id2mem.valid := io.if2id.valid && (instType(2) =/= mode_NOP)
  io.id2mem.memMode := instType(2)
  io.id2mem.addr := (io.rrs1.data.asSInt + imm).asUInt
  io.id2mem.data := io.rrs2.data
  io.id2mem.dst_id := io.if2id.inst(11, 7)
  io.id2mem.pc := io.if2id.pc
}

class Execute extends Module {
  val io = IO(new Bundle {
    val id2ex = Flipped(new ID2EX)
    val ex2wb = new EX2WB
  })

  val alu = Module(new ALU)
  alu.io.alu_op := io.id2ex.ctrl.aluOP
  alu.io.val1 := io.id2ex.rs1_d
  alu.io.val2 := io.id2ex.rs2_d
  
  val balu = Module(new BranchALU)
  balu.io.val1 := io.id2ex.rs1_d
  balu.io.val2 := io.id2ex.rs2_d
  balu.io.brType := io.id2ex.ctrl.brType

  val is_jmp = io.id2ex.jmpType === JMP_UNCOND || (io.id2ex.jmpType === JMP_COND && balu.io.is_jmp)
  val nextPC = Mux(is_jmp, io.id2ex.dst_d, io.id2ex.pc + 4.U)
  io.ex2wb.pc := io.id2ex.pc
  io.ex2wb.inst := io.id2ex.inst
  io.ex2wb.nextPC := nextPC
  io.ex2wb.is_jmp := is_jmp
  io.ex2wb.wreg.id := io.id2ex.dst_id
  io.ex2wb.wreg.data := Mux(io.id2ex.ctrl.wCsrEn, io.id2ex.rs2_d, alu.io.out)
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
  val state = RegInit(sIdle)
  val id_r = RegInit(0.U(REG_NUM_WIDTH.W))
  val valid_r = RegInit(false.B)
  val mode_r = RegInit(0.U(5.W))
  val addr_r = RegInit(0.U(ADDR_WIDTH.W))
  val pc_r = RegInit(0.U(ADDR_WIDTH.W))
  val inst_r = RegInit(0.U(INST_WIDTH.W))
	val wdata_r = RegInit(0.U(REG_WIDTH.W))
  switch(state) {
    is(sIdle) {
      when(io.id2mem.valid) {
        id_r := io.id2mem.dst_id
        mode_r := io.id2mem.memMode
        addr_r := io.id2mem.addr
        valid_r := true.B
        pc_r := io.id2mem.pc
        inst_r := io.id2mem.inst
				wdata_r := io.id2mem.data
      }
			when(io.memIO.ready && io.memIO.valid) {
				state := sWaitMem
			}
    }
    is(sWaitMem) {
      when(io.memIO.respValid) {
        state := sIdle
        valid_r := false.B
      }
    }
  }

  val curMode = Mux(state === sIdle, io.id2mem.memMode, mode_r)
  val curAddr = Mux(state === sIdle, io.id2mem.addr, addr_r)
  val memFinish = ((io.id2mem.valid && (state === sIdle)) || (state === sWaitMem)) && io.memIO.respValid
  val isMMIO = curAddr === "ha00003f8".U
  io.memIO.addr := Mux(valid_r, addr_r, io.id2mem.addr) & (~0x7.U(REG_WIDTH.W))
  io.memIO.wdata := Mux(valid_r, wdata_r, io.id2mem.data) << Cat(Mux(valid_r, addr_r(2, 0), io.id2mem.addr(2,0)), 0.U(3.W))
	io.memIO.valid := state === sIdle && (io.id2mem.valid || valid_r)
  io.memIO.wen :=  Mux(valid_r, mode_r(3), io.id2mem.memMode(3))
  io.memIO.wmask := mask_by_width(Mux(valid_r, mode_r(1,0), io.id2mem.memMode(1,0))) << Mux(valid_r, addr_r(2, 0), io.id2mem.addr(2,0))
	io.memIO.wsize := mode_r(1,0)

  io.mem2wb.valid := memFinish
  io.mem2wb.wreg.id := Mux(state === sIdle, io.id2mem.dst_id, id_r)
  io.mem2wb.wreg.data := rdata_by_mode(curMode, io.memIO.rdata >> Cat(curAddr(2,0), 0.U(3.W)))
  io.mem2wb.wreg.en := Mux(state === sIdle, io.id2mem.memMode(2), mode_r(2))
  io.mem2wb.pc := Mux(state === sIdle, io.id2mem.pc, pc_r)
  io.mem2wb.inst := Mux(state === sIdle, io.id2mem.inst, inst_r)
  io.mem2wb.nextPC := io.mem2wb.pc + 4.U
  io.mem2wb.isMMIO := isMMIO
 }

class WriteBack extends Module {
  val io = IO(new Bundle {
    val ex2wb = Flipped(new EX2WB)
    val mem2wb = Flipped(new MEM2WB)
    val instFinish = Output(Bool())
    val wreg = Flipped(new WriteReg)
    val wcsr = Flipped(new WriteCSR)
    val nextPC = Output(UInt(ADDR_WIDTH.W))
  })
  val prevFinish = RegInit(false.B)
  prevFinish := io.instFinish
  val prevPC = RegNext(Mux(io.ex2wb.valid, io.ex2wb.pc, io.mem2wb.pc))
  val prevInst = RegNext(Mux(io.ex2wb.valid, io.ex2wb.inst, io.mem2wb.inst))
  val prevMMIO = RegNext(io.mem2wb.isMMIO)

  io.instFinish := io.ex2wb.valid || io.mem2wb.valid
  io.wreg.id := Mux(io.ex2wb.valid, io.ex2wb.wreg.id, io.mem2wb.wreg.id)
  io.wreg.data := Mux(io.ex2wb.valid, io.ex2wb.wreg.data, io.mem2wb.wreg.data)
  io.wreg.en := (io.ex2wb.valid && io.ex2wb.wreg.en ) || (io.mem2wb.valid && io.mem2wb.wreg.en )

  io.wcsr := io.ex2wb.wcsr
  io.wcsr.en := io.ex2wb.wcsr.en && io.ex2wb.valid

  io.nextPC := Mux(io.ex2wb.valid, io.ex2wb.nextPC, io.mem2wb.nextPC)

  val updateInst = Module(new UpdateInst)
  updateInst.io.valid := prevFinish
  updateInst.io.pc := prevPC
  updateInst.io.inst := prevInst
  updateInst.io.clock := clock
  updateInst.io.isMMIO := prevMMIO
}

class rv32e extends Module {
  val io = IO(new Bundle {
    val out = new AXIIO
  })
  dontTouch(io)

  val fetch = Module(new Fetch)
  val decode = Module(new Decode)
  val execute = Module(new Execute)
  val memory = Module(new Memory)
  val writeback = Module(new WriteBack)
  val regs = Module(new Regs)
  val csrs = Module(new Csrs)
	val axiBridge = Module(new AXIBridge)
	val crossbar = Module(new SimpleBusCrossbar)
  
  fetch.io.nextPC <> writeback.io.nextPC
  fetch.io.instFinish <> writeback.io.instFinish
  fetch.io.simpleBus <> crossbar.io.in1
  fetch.io.if2id <> decode.io.if2id

  decode.io.id2ex <> execute.io.id2ex
  decode.io.id2mem <> memory.io.id2mem
  decode.io.rrs1 <> regs.io.rs1
  decode.io.rrs2 <> regs.io.rs2
  decode.io.rcsr <> csrs.io.rs

  execute.io.ex2wb <> writeback.io.ex2wb

  memory.io.mem2wb <> writeback.io.mem2wb
  memory.io.memIO <> crossbar.io.in2
  
  writeback.io.wreg <> regs.io.dst
  writeback.io.wreg <> csrs.io.rd
	crossbar.io.out <> axiBridge.io.in
	axiBridge.io.out <> io.out
}

class AXIBridge extends Module {
	val io = IO(new Bundle {
		val in = Flipped(new SimpleBus)
		val out = new AXIIO
	})
	val sIdle :: sWaddr :: sWdata :: sWresp :: sRaddr :: sRdata :: sFinish :: Nil = Enum(7)
	val state = RegInit(sIdle)

	val addr = RegInit(0.U(ADDR_WIDTH.W))
	val wdata = RegInit(0.U(64.W))
	val wmask = RegInit(0.U(8.W))
	val wsize = RegInit(0.U(3.W))

	val waddrEn = RegInit(false.B)
	val wdataEn = RegInit(false.B)
	val wstrb   = RegInit(0.U(8.W))

	val raddrEn = RegInit(false.B)
	val rdataEn = RegInit(false.B)

	io.in.ready := false.B
	io.in.rdata := 0.U
	io.in.respValid := false.B

	switch(state) {
		is(sIdle) {
			when(io.in.valid) {
				io.in.ready := true.B
				addr := io.in.addr
				wdata := io.in.wdata
				wmask := io.in.wmask
				wsize := io.in.wsize
				when(io.in.wen) {
					state := sWaddr
					waddrEn := true.B
				}.otherwise {
					state := sRaddr
					raddrEn := true.B
				}
			}
		}
		is(sWaddr) {
			when(waddrEn && io.out.awready) {
				waddrEn := false.B
				state := sWdata
				wdataEn := true.B
			}
		}
		is(sWdata) {
			when(wdataEn && io.out.wready) {
				state := sIdle
				wdataEn := false.B
				io.in.respValid := true.B
			}
		}
		is(sRaddr) {
			when(raddrEn && io.out.arready) {
				raddrEn := false.B
				state := sRdata
				rdataEn := true.B
			}
		}
		is(sRdata) {
			when(rdataEn && io.out.rvalid) {
				io.in.rdata := io.out.rdata
				io.in.respValid := true.B
				state := sIdle
			}
		}
	}
	io.out.awvalid 	:= waddrEn
	io.out.awaddr 	:= addr
	io.out.awid 		:= 0.U
	io.out.awlen		:= 0.U
	io.out.awsize   := wsize
	io.out.awburst	:= BURST_INCR
	io.out.wvalid 	:= wdataEn
	io.out.wdata		:= wdata
	io.out.wstrb		:= wmask
	io.out.wlast		:= true.B
	io.out.bready		:= true.B
	io.out.arvalid	:= raddrEn
	io.out.araddr		:= addr
	io.out.arid			:= 0.U
	io.out.arlen 		:= 0.U
	io.out.arsize		:= 0.U
	io.out.arburst	:= BURST_INCR
	io.out.rready		:= rdataEn
}

class SimpleBusCrossbar extends Module {
    val io = IO(new Bundle {
        val in1 = Flipped(new SimpleBus)
        val in2 = Flipped(new SimpleBus)
        val out = new SimpleBus
    })
    val sIdle :: sIn1 :: sIn2 :: Nil = Enum(3)
    val state = RegInit(sIdle)

		io.in1.ready := false.B
		io.in1.rdata := 0.U
		io.in1.respValid := false.B
		io.in2.ready := false.B
		io.in2.rdata := 0.U
		io.in2.respValid := false.B
		io.out.addr := 0.U
		io.out.wdata := 0.U
		io.out.wmask := 0.U
		io.out.wsize := 0.U
		io.out.wen := 0.U
		io.out.valid := 0.U

    switch(state) {
			is(sIdle) {
				when(io.in1.valid) {
					io.in1 <> io.out
					when(io.in1.ready) {
						state := sIn1
					}
				}.elsewhen(io.in2.valid) {
					io.in2 <> io.out
					when(io.in2.ready) {
						state := sIn2
					}
				}
			}
			is(sIn1) {
				io.in1 <> io.out
				when(io.out.respValid) {
					state := sIdle
				}
			}
			is(sIn2) {
				io.in2 <> io.out
				when(io.out.respValid) {
					state := sIdle
				}
			}
		}
}