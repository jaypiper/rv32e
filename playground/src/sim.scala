import chisel3._
import chisel3.util._
import cpu._
import io._

class simMem extends Module {
  val io = IO(new Bundle{
    val instIO = Flipped(new SimpleBus)
    val memIO = Flipped(new SimpleBus)
  })

  val ram = Mem(0x10000000, UInt(8.W))
  val inst_rdata = Cat((0 until 4).reverse.map(i => ram((io.instIO.addr + i.U) & 0xfffffff.U)))
  val mem_rdata = Cat((0 until 4).reverse.map(i => ram((io.memIO.addr + i.U) & 0xfffffff.U)))
  io.instIO.respValid := io.instIO.valid
  io.memIO.respValid := io.memIO.valid
  when(io.memIO.addr(31) && io.memIO.wen && io.memIO.valid) {
    for(i <- 0 until 4) {
      ram(io.memIO.addr + i.U) := Mux(io.memIO.wmask(i) === 1.U, io.memIO.wdata(8*i+7, 8*i), ram(io.memIO.addr + i.U))
    }
  } .elsewhen(io.memIO.valid && !io.memIO.addr(31)) {
    printf("invalid addr 0x%x\n", io.memIO.addr);
  }
  io.instIO.rdata := inst_rdata
  io.memIO.rdata := mem_rdata
}

class top extends Module {
  val io = IO(new Bundle{    
  })

  val rv32e = Module(new rv32e)
  val mem = Module(new simMem)
  rv32e.io.master1 <> mem.io.instIO
  rv32e.io.master2 <> mem.io.memIO
}