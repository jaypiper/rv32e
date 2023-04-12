class simMem extends Module {
  val io = IO(Flipped(new SimpleBus))

  val ram = Mem(0x10000000, UInt(8.W))
  val rdata := Cat((0 until 4).reverse.map(i => ram((io.addr + i.U) & 0xfffffff.U)))
  when(io.valid) io.respValid := true
  when(io.wen && io.valid) {
    for(i <- 0 until 4) {
      ram(io.addr + i.U) := Mux(io.wmask(i) === 1.U, io.wdata(8*i+7, 8*i), ram(io.addr + i.U))
    }
    ram()
  }
}

class top extends Module {
  val io = IO(new Bundle{    
  })

  val rv32e = Module(new rv32e)
  val mem = Module(new simMem)
  rv32e.master <> mem.io
}