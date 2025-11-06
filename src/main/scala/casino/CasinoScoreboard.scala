package casino

import chisel3._
import chisel3.util._

class ScoreboardAllocate(params: CasinoParams) extends Bundle {
  val dst = UInt(log2Ceil(params.physicalRegisters).W)
  val latency = UInt(params.scoreboardDelayBits.W)
  val producer = UInt(params.producerBits.W)
  val branchTag = UInt(6.W)
  override def cloneType: this.type = new ScoreboardAllocate(params).asInstanceOf[this.type]
}

class ScoreboardWakeup(params: CasinoParams) extends Bundle {
  val dst = UInt(log2Ceil(params.physicalRegisters).W)
  val branchTag = UInt(6.W)
  override def cloneType: this.type = new ScoreboardWakeup(params).asInstanceOf[this.type]
}

class ScoreboardFlush extends Bundle {
  val branchTag = UInt(6.W)
}

class ScoreboardEntry(params: CasinoParams) extends Bundle {
  val ready = Bool()
  val issue = Bool()
  val delay = UInt(params.scoreboardDelayBits.W)
  val producerCount = UInt(params.producerBits.W)
  val branchTag = UInt(6.W)
  override def cloneType: this.type = new ScoreboardEntry(params).asInstanceOf[this.type]
}

/**
 * Scoreboard tracking per-physical-register readiness. Implements
 * ready/issue/delay/producer-count semantics to eliminate global wakeup.
 */
class CasinoScoreboard(params: CasinoParams, queryPorts: Int = 4) extends Module {
  val physWidth = log2Ceil(params.physicalRegisters)
  val io = IO(new Bundle {
    val queries = Input(Vec(queryPorts, UInt(physWidth.W)))
    val queryReady = Output(Vec(queryPorts, Bool()))
    val allocate = Flipped(Valid(new ScoreboardAllocate(params)))
    val wakeup = Flipped(Valid(new ScoreboardWakeup(params)))
    val consume = Flipped(Valid(UInt(physWidth.W)))
    val flush = Flipped(Valid(new ScoreboardFlush))
  })

  val entries = RegInit(VecInit(Seq.fill(params.physicalRegisters) {
    val init = Wire(new ScoreboardEntry(params))
    init.ready := true.B
    init.issue := false.B
    init.delay := 0.U
    init.producerCount := 0.U
    init.branchTag := 0.U
    init
  }))

  when(io.flush.valid) {
    for (i <- 0 until params.physicalRegisters) {
      when(entries(i).branchTag === io.flush.bits.branchTag) {
        entries(i).ready := true.B
        entries(i).issue := false.B
        entries(i).delay := 0.U
        entries(i).producerCount := 0.U
      }
    }
  }

  when(io.allocate.valid) {
    val dst = io.allocate.bits.dst
    val entry = entries(dst)
    entry.ready := false.B
    entry.issue := true.B
    entry.delay := io.allocate.bits.latency
    entry.producerCount := io.allocate.bits.producer
    entry.branchTag := io.allocate.bits.branchTag
    entries(dst) := entry
  }

  when(io.wakeup.valid) {
    val dst = io.wakeup.bits.dst
    val entry = entries(dst)
    when(entry.producerCount === 0.U) {
      entry.ready := true.B
    }
    entry.issue := false.B
    entry.delay := 0.U
    entries(dst) := entry
  }

  when(io.consume.valid) {
    val dst = io.consume.bits
    val entry = entries(dst)
    when(entry.producerCount =/= 0.U) {
      entry.producerCount := entry.producerCount - 1.U
      when(entry.producerCount === 1.U) {
        entry.ready := true.B
      }
      entries(dst) := entry
    }
  }

  for (i <- 0 until params.physicalRegisters) {
    val entry = entries(i)
    when(entry.issue && entry.delay =/= 0.U) {
      entry.delay := entry.delay - 1.U
      when(entry.delay === 1.U && entry.producerCount === 0.U) {
        entry.ready := true.B
      }
      entries(i) := entry
    }
  }

  io.queryReady := io.queries.map(entries(_).ready)
}
