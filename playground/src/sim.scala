import chisel3._
import chisel3.util._
import cpu._
import io._
import params.common._
import params.axi_config._

class simMem extends Module{
    val io = IO(new Bundle {
      val memAxi = Flipped(new AXIIO)
    })
    val (sIdle :: sWdata :: sWresp :: sRdata:: Nil) = Enum(4)
    val ram = Mem(0x10000000, UInt(8.W))

    val burstLen = RegInit(0.U(8.W))
    val offset  = RegInit(0.U(8.W))

    val waReady = RegInit(false.B)
    val wdReady = RegInit(false.B)
    val waStart = RegInit(0.U(ADDR_WIDTH.W))
    val waddr   = (waStart + offset * 8.U) & 0xfffffff.U

    val raReady = RegInit(false.B)
    val raStart = RegInit(0.U(ADDR_WIDTH.W))
    val rdValid = RegInit(false.B)
    val rdata   = Cat((0 until 8).reverse.map(i => ram((raStart + offset * 8.U + i.U)&0xfffffff.U)))

    val state = RegInit(sIdle)
    val isLast  = (offset >= burstLen)

    switch(state){
        is(sIdle){  //(接收地址信息)
            waReady := true.B
            raReady := true.B
            offset  := 0.U
            when(io.memAxi.awvalid && waReady){
                state   := sWdata
                waStart   := io.memAxi.awaddr
                burstLen := io.memAxi.awlen
                waReady := false.B
                wdReady := true.B
            }
            when(io.memAxi.arvalid && raReady){
                state   := sRdata
                raStart   := io.memAxi.araddr
                burstLen := io.memAxi.arlen
                raReady := false.B
                rdValid := true.B
            }
        }
        //write
        is(sWdata){
            when(io.memAxi.wvalid){
              when(waStart === "ha00003f8".U) {
                printf("%c", io.memAxi.wdata(7,0))
              }.otherwise {
                for(i <- 0 until 8){
                  ram(waddr + i.U) := Mux(io.memAxi.wstrb(i) === 1.U, io.memAxi.wdata(8*i+7, 8*i), ram(waddr + i.U))
                }
                offset := offset + 1.U
              }
              when(io.memAxi.wlast){
                  wdReady := false.B
                  state   := sIdle
              }
            }
        }
        //read
        is(sRdata){
            rdValid := true.B
            when(rdValid && io.memAxi.rready){
                offset  := offset + 1.U
                rdValid := false.B
                when(isLast){
                    state := sIdle
                }
                // printf("[slave-read] addr: %x data: %x\n", raStart + offset * 8.U, rdata)
            }
        }
    }
    // printf("expected: data: %x\n", Cat((0 until 8).reverse.map(i => ram("h106958".U + i.U))))
    io.memAxi.rresp := 0.U
    io.memAxi.rid := 0.U
    io.memAxi.bvalid := true.B
    io.memAxi.bresp := RESP_OKAY
    io.memAxi.bid := 0.U
    io.memAxi.arready := raReady
    io.memAxi.awready := waReady
    io.memAxi.wready := wdReady
    io.memAxi.rvalid := rdValid
    io.memAxi.rdata := rdata
    io.memAxi.rlast := isLast

    dontTouch(io.memAxi)
}


class top extends Module {
  val io = IO(new Bundle{    
  })

  val rv32e = Module(new rv32e)
  val mem = Module(new simMem)
  rv32e.io.out <> mem.io.memAxi
}