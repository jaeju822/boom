//******************************************************************************
// CASINO support infrastructure for BOOM
//------------------------------------------------------------------------------

package boom.casino

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters

import boom.common._

/**
 * Identify instructions that can be issued speculatively from the Speculative IQ.
 * The implementation is intentionally lightweight and relies on the ready bits
 * provided by the rename stage to approximate the scheduling window described in
 * the CASINO paper.
 */
class SpecInOStage(window: Int)(implicit p: Parameters) extends BoomModule {
  require(window >= 1)

  val io = IO(new Bundle {
    val valid = Input(Vec(coreWidth, Bool()))
    val uops  = Input(Vec(coreWidth, new MicroOp()))
    val readyMask = Output(Vec(coreWidth, Bool()))
  })

  for (w <- 0 until coreWidth) {
    val uop = io.uops(w)
    val ready = !uop.prs1_busy && !uop.prs2_busy && !uop.prs3_busy && !uop.ppred_busy
    io.readyMask(w) := io.valid(w) && ready
  }
}

/**
 * Data buffer used by the in-order portion of the CASINO pipeline.  The
 * simplified model tracks occupancy to provide backpressure when the buffer is
 * full, mimicking the behaviour of the precise data buffer described in the
 * paper.
 */
class CasinoDataBuffer(entries: Int)(implicit p: Parameters) extends BoomModule {
  require(entries > 0)

  private val countWidth = log2Ceil(entries + 1)

  val io = IO(new Bundle {
    val allocate = Input(Vec(coreWidth, Bool()))
    val release  = Input(Vec(coreWidth, Bool()))
    val occupancy = Output(UInt(countWidth.W))
    val full = Output(Bool())
  })

  val count = RegInit(0.U(countWidth.W))

  val allocs   = PopCount(io.allocate)
  val releases = PopCount(io.release)

  val afterAlloc = count +& allocs
  val releasesExt = releases.asUInt
  val afterRelease = Mux(afterAlloc >= releasesExt, afterAlloc - releasesExt, 0.U)
  val entriesMax = entries.U((countWidth + 1).W)
  val limited = Mux(afterRelease > entriesMax, entriesMax, afterRelease)

  count := limited(countWidth-1, 0)
  io.occupancy := count
  io.full := count === entries.U
}

/**
 * Outstanding store counter array used to avoid unnecessary associative
 * searches in the LSU.  The implementation keeps a small set of saturating
 * counters indexed by a hash of the store address (or, in this simplified
 * version, a synthetic hash provided by the core).
 */
class OutstandingStoreCounterArray(entries: Int, lanes: Int)(implicit p: Parameters) extends BoomModule {
  require(entries > 0)
  require(isPow2(entries))
  require(lanes > 0)

  private val idxWidth = log2Ceil(entries)
  private val counterWidth = 3

  val io = IO(new Bundle {
    val increment = Input(Vec(lanes, Valid(UInt(idxWidth.W))))
    val decrement = Input(Vec(lanes, Valid(UInt(idxWidth.W))))
    val query     = Input(Vec(lanes, Valid(UInt(idxWidth.W))))
    val queryHasStore = Output(Vec(lanes, Bool()))
    val snapshot  = Output(Vec(entries, UInt(counterWidth.W)))
  })

  val counters = RegInit(VecInit(Seq.fill(entries)(0.U(counterWidth.W))))
  val nextCounters = WireInit(counters)

  for (l <- 0 until lanes) {
    when (io.increment(l).valid) {
      val idx = io.increment(l).bits
      val value = nextCounters(idx)
      nextCounters(idx) := Mux(value === ((1 << counterWidth) - 1).U, value, value + 1.U)
    }
    when (io.decrement(l).valid) {
      val idx = io.decrement(l).bits
      val value = nextCounters(idx)
      nextCounters(idx) := Mux(value === 0.U, value, value - 1.U)
    }
  }

  counters := nextCounters

  for (l <- 0 until lanes) {
    val idx = io.query(l).bits
    val value = Mux(io.query(l).valid, counters(idx), 0.U)
    io.queryHasStore(l) := io.query(l).valid && value =/= 0.U
  }

  io.snapshot := counters
}
