package casino

import chisel3._
import chisel3.util._

/** Parameter bundle configuring the CASINO microarchitecture. */
case class CasinoParams(
    speculativeWindow: Int = 4,
    inOrderWindow: Int = 12,
    dataBufferEntries: Int = 4,
    physicalRegisters: Int = 32,
    outstandingStoreCounters: Int = 64,
    scoreboardDelayBits: Int = 4,
    producerBits: Int = 2,
    oscaIndexBits: Int = 6) {
  require(speculativeWindow > 0)
  require(inOrderWindow > 0)
  require(dataBufferEntries > 0)
  require(physicalRegisters > 0)
  require(outstandingStoreCounters > 0)
  require(scoreboardDelayBits > 0)
  require(producerBits > 0)
  require(oscaIndexBits > 0)
}

object CasinoParams {
  val default: CasinoParams = CasinoParams()
}

/** Architectural micro-op definition used throughout the CASINO datapath. */
class CasinoMicroOp(params: CasinoParams) extends Bundle {
  val robIdx = UInt(8.W)
  val src0 = UInt(log2Ceil(params.physicalRegisters).W)
  val src1 = UInt(log2Ceil(params.physicalRegisters).W)
  val dst = UInt(log2Ceil(params.physicalRegisters).W)
  val dstArch = UInt(5.W)
  val func = UInt(8.W)
  val isLoad = Bool()
  val isStore = Bool()
  val latency = UInt(params.scoreboardDelayBits.W)
  val branchTag = UInt(6.W)
  val speculative = Bool()
  val data = UInt(64.W)

  override def cloneType: this.type = new CasinoMicroOp(params).asInstanceOf[this.type]
}

object CasinoMicroOp {
  def default(params: CasinoParams): CasinoMicroOp = {
    val op = Wire(new CasinoMicroOp(params))
    op := 0.U.asTypeOf(new CasinoMicroOp(params))
    op
  }
}
