package casino

import chisel3._
import chisel3.util._

/**
 * Top-level CASINO core skeleton that connects the selective issue windows,
 * scoreboard, rename logic, data buffer, and OSCA.
 */
class CasinoCore(params: CasinoParams = CasinoParams.default) extends Module {
  val physWidth = log2Ceil(params.physicalRegisters)
  val io = IO(new Bundle {
    val frontendEnq = Flipped(Decoupled(new CasinoMicroOp(params)))
    val commit = Flipped(Decoupled(UInt(8.W)))
    val lsuQuery = Input(UInt(params.oscaIndexBits.W))
    val lsuQueryValid = Input(Bool())
    val lsuBlocked = Output(Bool())
    val flush = Flipped(Valid(new ScoreboardFlush))
  })

  val scoreboard = Module(new CasinoScoreboard(params, queryPorts = 4))
  val siq = Module(new SpecInOQueue(params))
  val iq = Module(new InOrderIssueQueue(params))
  val dataBuffer = Module(new CasinoDataBuffer(params))
  val rename = Module(new RenamePipeline(params))
  val recovery = Module(new RecoveryLog(params))
  val osca = Module(new OutstandingStoreCounterArray(params))
  val iqInputArb = Module(new Arbiter(new CasinoMicroOp(params), 2))

  siq.io.flush := io.flush
  iq.io.flush := io.flush
  dataBuffer.io.flush := io.flush
  rename.io.flush := io.flush
  recovery.io.flush := io.flush
  osca.io.flush := io.flush
  scoreboard.io.flush := io.flush

  siq.io.enq <> io.frontendEnq

  val scoreboardQueries = Wire(Vec(4, UInt(physWidth.W)))
  scoreboardQueries(0) := rename.io.scoreboardQueries(0)
  scoreboardQueries(1) := rename.io.scoreboardQueries(1)
  scoreboardQueries(2) := Mux(siq.io.headValid, siq.io.headSources(0), 0.U)
  scoreboardQueries(3) := Mux(siq.io.headValid, siq.io.headSources(1), 0.U)
  scoreboard.io.queries := scoreboardQueries

  rename.io.scoreboardReady := VecInit(scoreboard.io.queryReady(0), scoreboard.io.queryReady(1))
  siq.io.readyHints := VecInit(scoreboard.io.queryReady(2), scoreboard.io.queryReady(3))

  rename.io.req.valid := siq.io.issue.valid
  rename.io.req.bits := siq.io.issue.bits
  siq.io.issue.ready := rename.io.req.ready

  iqInputArb.io.in(0) <> rename.io.resp
  iqInputArb.io.in(1) <> siq.io.promote
  iq.io.enq <> iqInputArb.io.out

  iq.io.issue.ready := dataBuffer.io.allocate.ready
  dataBuffer.io.allocate.valid := iq.io.issue.valid
  dataBuffer.io.allocate.bits := iq.io.issue.bits

  dataBuffer.io.read.ready := true.B
  dataBuffer.io.commit <> io.commit

  val freeReg = RegInit(0.U(physWidth.W))
  rename.io.freelistAlloc.valid := true.B
  rename.io.freelistAlloc.bits := freeReg
  when(rename.io.freelistAlloc.ready && rename.io.freelistAlloc.valid) {
    freeReg := freeReg + 1.U
  }

  rename.io.freelistFree.ready := true.B

  recovery.io.push <> rename.io.recoveryPush
  rename.io.recoveryPop <> recovery.io.pop

  scoreboard.io.allocate.valid := rename.io.scoreboardAllocate.valid
  scoreboard.io.allocate.bits := rename.io.scoreboardAllocate.bits
  scoreboard.io.consume.valid := rename.io.scoreboardConsume.valid
  scoreboard.io.consume.bits := rename.io.scoreboardConsume.bits

  scoreboard.io.wakeup.valid := dataBuffer.io.read.valid && dataBuffer.io.read.ready
  scoreboard.io.wakeup.bits.dst := dataBuffer.io.read.bits.dst
  scoreboard.io.wakeup.bits.branchTag := dataBuffer.io.read.bits.branchTag

  osca.io.query := io.lsuQuery
  osca.io.queryValid := io.lsuQueryValid
  osca.io.allocate.valid := dataBuffer.io.allocate.fire && dataBuffer.io.allocate.bits.isStore
  osca.io.allocate.bits := dataBuffer.io.allocate.bits.robIdx(params.oscaIndexBits - 1, 0)
  osca.io.release.valid := io.commit.fire
  osca.io.release.bits := io.commit.bits(params.oscaIndexBits - 1, 0)

  io.lsuBlocked := osca.io.queryResp
}
