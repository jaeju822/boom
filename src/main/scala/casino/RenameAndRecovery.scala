package casino

import chisel3._
import chisel3.util._

class RecoveryLogEntry(params: CasinoParams) extends Bundle {
  val arch = UInt(5.W)
  val oldPhys = UInt(log2Ceil(params.physicalRegisters).W)
  val branchTag = UInt(6.W)
  override def cloneType: this.type = new RecoveryLogEntry(params).asInstanceOf[this.type]
}

class RecoveryLog(params: CasinoParams) extends Module {
  val io = IO(new Bundle {
    val push = Flipped(Decoupled(new RecoveryLogEntry(params)))
    val pop = Decoupled(new RecoveryLogEntry(params))
    val flush = Flipped(Valid(new ScoreboardFlush))
  })

  val depth = params.speculativeWindow * 2
  val entries = Reg(Vec(depth, new RecoveryLogEntry(params)))
  val head = RegInit(0.U(log2Ceil(depth).W))
  val tail = RegInit(0.U(log2Ceil(depth).W))
  val maybeFull = RegInit(false.B)

  def isEmpty = head === tail && !maybeFull
  def isFull = head === tail && maybeFull

  when(io.flush.valid) {
    head := 0.U
    tail := 0.U
    maybeFull := false.B
  }

  io.push.ready := !isFull
  when(io.push.fire) {
    entries(tail) := io.push.bits
    tail := tail + 1.U
    when(tail + 1.U === head) { maybeFull := true.B }
  }

  io.pop.valid := !isEmpty
  io.pop.bits := entries(head)
  when(io.pop.fire) {
    head := head + 1.U
    maybeFull := false.B
  }
}

/** Conditional renaming pipeline that allocates physical registers only for ready ops. */
class RenamePipeline(params: CasinoParams) extends Module {
  val physWidth = log2Ceil(params.physicalRegisters)
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new CasinoMicroOp(params)))
    val resp = Decoupled(new CasinoMicroOp(params))
    val scoreboardQueries = Output(Vec(2, UInt(physWidth.W)))
    val scoreboardReady = Input(Vec(2, Bool()))
    val scoreboardAllocate = Valid(new ScoreboardAllocate(params))
    val scoreboardConsume = Valid(UInt(physWidth.W))
    val freelistAlloc = Flipped(Decoupled(UInt(physWidth.W)))
    val freelistFree = Decoupled(UInt(physWidth.W))
    val recoveryPush = Decoupled(new RecoveryLogEntry(params))
    val recoveryPop = Flipped(Decoupled(new RecoveryLogEntry(params)))
    val flush = Flipped(Valid(new ScoreboardFlush))
  })

  val holding = RegInit(false.B)
  val saved = Reg(new CasinoMicroOp(params))

  when(io.flush.valid) {
    holding := false.B
  }

  when(io.req.fire) {
    saved := io.req.bits
    holding := true.B
  }

  io.req.ready := !holding

  io.scoreboardQueries := VecInit(saved.src0, saved.src1)

  val sourcesReady = io.scoreboardReady.reduce(_ && _)
  val canRename = holding && sourcesReady && io.freelistAlloc.valid

  io.freelistAlloc.ready := holding && sourcesReady && io.resp.ready

  val allocateWire = Wire(Valid(new ScoreboardAllocate(params)))
  allocateWire.valid := false.B
  allocateWire.bits := 0.U.asTypeOf(new ScoreboardAllocate(params))

  val consumeWire = Wire(Valid(UInt(physWidth.W)))
  consumeWire.valid := false.B
  consumeWire.bits := saved.dst

  io.freelistFree.valid := false.B
  io.freelistFree.bits := 0.U
  io.recoveryPush.valid := false.B
  io.recoveryPush.bits := 0.U.asTypeOf(new RecoveryLogEntry(params))
  io.recoveryPop.ready := false.B

  when(canRename && io.resp.ready) {
    val newPhys = io.freelistAlloc.bits
    allocateWire.valid := true.B
    allocateWire.bits.dst := newPhys
    allocateWire.bits.latency := saved.latency
    allocateWire.bits.producer := 1.U
    allocateWire.bits.branchTag := saved.branchTag
    consumeWire.valid := true.B
    consumeWire.bits := newPhys
    io.recoveryPush.valid := true.B
    io.recoveryPush.bits.arch := saved.dstArch
    io.recoveryPush.bits.oldPhys := saved.dst
    io.recoveryPush.bits.branchTag := saved.branchTag
    saved.dst := newPhys
  }

  io.resp.valid := holding && sourcesReady
  io.resp.bits := saved

  when(io.resp.fire) {
    holding := false.B
  }

  io.scoreboardAllocate := allocateWire
  io.scoreboardConsume := consumeWire
}
