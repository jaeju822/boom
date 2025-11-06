package casino

import chisel3._
import chisel3.util._

/** Speculative Issue Queue that selectively renames ready operations. */
class SpecInOQueue(params: CasinoParams) extends Module {
  val physWidth = log2Ceil(params.physicalRegisters)
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new CasinoMicroOp(params)))
    val promote = Decoupled(new CasinoMicroOp(params))
    val issue = Decoupled(new CasinoMicroOp(params))
    val headSources = Output(Vec(2, UInt(physWidth.W)))
    val headValid = Output(Bool())
    val readyHints = Input(Vec(2, Bool()))
    val flush = Flipped(Valid(new ScoreboardFlush))
  })

  val entries = Reg(Vec(params.speculativeWindow, new CasinoMicroOp(params)))
  val valids = RegInit(VecInit(Seq.fill(params.speculativeWindow)(false.B)))
  val head = RegInit(0.U(log2Ceil(params.speculativeWindow).W))
  val tail = RegInit(0.U(log2Ceil(params.speculativeWindow).W))
  val maybeFull = RegInit(false.B)

  def isFull: Bool = head === tail && maybeFull

  when(io.flush.valid) {
    valids.foreach(_ := false.B)
    head := 0.U
    tail := 0.U
    maybeFull := false.B
  }

  io.enq.ready := !isFull
  when(io.enq.fire) {
    entries(tail) := io.enq.bits
    valids(tail) := true.B
    tail := tail + 1.U
    when(tail + 1.U === head) { maybeFull := true.B }
  }

  val headIdx = head
  val headValid = valids(headIdx)
  val headOp = entries(headIdx)

  io.headValid := headValid
  io.headSources := VecInit(headOp.src0, headOp.src1)

  val readyForIssue = headValid && io.readyHints.reduce(_ && _)

  io.issue.valid := readyForIssue
  io.issue.bits := headOp

  val promoteNeeded = headValid && !readyForIssue
  io.promote.valid := promoteNeeded
  io.promote.bits := headOp

  when(readyForIssue || promoteNeeded) {
    valids(headIdx) := false.B
    head := head + 1.U
    maybeFull := false.B
  }
}

/** In-order issue queue that consumes promoted operations sequentially. */
class InOrderIssueQueue(params: CasinoParams) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(new CasinoMicroOp(params)))
    val issue = Decoupled(new CasinoMicroOp(params))
    val dataBufferBusy = Output(Bool())
    val flush = Flipped(Valid(new ScoreboardFlush))
  })

  val queue = Module(new Queue(new CasinoMicroOp(params), params.inOrderWindow, hasFlush = true))
  queue.io.enq <> io.enq
  queue.io.flush.valid := io.flush.valid
  queue.io.flush.bits := io.flush.bits
  io.issue <> queue.io.deq
  io.dataBufferBusy := queue.io.count === (params.inOrderWindow - 1).U
}

/**
 * Data buffer that holds results from the in-order window until commit.
 */
class CasinoDataBuffer(params: CasinoParams) extends Module {
  val io = IO(new Bundle {
    val allocate = Flipped(Decoupled(new CasinoMicroOp(params)))
    val read = Decoupled(new CasinoMicroOp(params))
    val commit = Flipped(Decoupled(UInt(8.W)))
    val flush = Flipped(Valid(new ScoreboardFlush))
  })

  val entries = Reg(Vec(params.dataBufferEntries, new CasinoMicroOp(params)))
  val valids = RegInit(VecInit(Seq.fill(params.dataBufferEntries)(false.B)))
  val head = RegInit(0.U(log2Ceil(params.dataBufferEntries).W))
  val tail = RegInit(0.U(log2Ceil(params.dataBufferEntries).W))
  val maybeFull = RegInit(false.B)

  def isEmpty = head === tail && !maybeFull
  def isFull = head === tail && maybeFull

  when(io.flush.valid) {
    valids.foreach(_ := false.B)
    head := 0.U
    tail := 0.U
    maybeFull := false.B
  }

  io.allocate.ready := !isFull
  when(io.allocate.fire) {
    entries(tail) := io.allocate.bits
    valids(tail) := true.B
    tail := tail + 1.U
    when(tail + 1.U === head) { maybeFull := true.B }
  }

  io.read.valid := !isEmpty
  io.read.bits := entries(head)
  when(io.read.fire) {
    valids(head) := false.B
    head := head + 1.U
    maybeFull := false.B
  }

  io.commit.ready := !isEmpty
  when(io.commit.fire) {
    val commitIdx = io.commit.bits
    for (i <- 0 until params.dataBufferEntries) {
      when(valids(i) && entries(i).robIdx === commitIdx) {
        valids(i) := false.B
      }
    }
  }
}
