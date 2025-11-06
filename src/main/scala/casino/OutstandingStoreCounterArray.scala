package casino

import chisel3._
import chisel3.util._

class OutstandingStoreCounterArray(params: CasinoParams) extends Module {
  val io = IO(new Bundle {
    val allocate = Flipped(Decoupled(UInt(params.oscaIndexBits.W)))
    val release = Flipped(Decoupled(UInt(params.oscaIndexBits.W)))
    val query = Input(UInt(params.oscaIndexBits.W))
    val queryValid = Input(Bool())
    val queryResp = Output(Bool())
    val flush = Flipped(Valid(new ScoreboardFlush))
  })

  val counters = RegInit(VecInit(Seq.fill(params.outstandingStoreCounters)(0.U(4.W))))

  when(io.flush.valid) {
    counters.foreach(_ := 0.U)
  }

  io.allocate.ready := true.B
  when(io.allocate.fire) {
    val idx = io.allocate.bits
    counters(idx) := counters(idx) + 1.U
  }

  io.release.ready := true.B
  when(io.release.fire) {
    val idx = io.release.bits
    when(counters(idx) =/= 0.U) {
      counters(idx) := counters(idx) - 1.U
    }
  }

  io.queryResp := Mux(io.queryValid, counters(io.query) =/= 0.U, false.B)
}
