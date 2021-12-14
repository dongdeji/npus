/*
 * 
 * A UART is a serial port, also called an RS232 interface.
 * 
 */

package npus


import chisel3._
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import chipsalliance.rocketchip.config._
import chipsalliance.rocketchip.config.Config
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.GenericLogicalTreeNode


class RRScheduler extends Module with NpusParams
{
  val io = IO(new Bundle() {
                    val readys = Input(UInt(numThread.W))
                    val issues = Output(UInt(numThread.W))
                    })

  val issues_cnt = PopCount(io.issues)
  //if(numThread > 1) { assert((io.readys =/= (-1.S(numThread.W)).asUInt)) } // can not be all idle
  assert(issues_cnt === 1.U || issues_cnt === 0.U) // one thread per clock, otherwise error

  val rrcnt = RegInit(0.U((log2Up(2*numThread)).W))
  val rrcnt_nxt = Wire(UInt((log2Up(2*numThread)).W))
  rrcnt := rrcnt_nxt

  val expend_ready = Wire(UInt((2*numThread).W));chisel3.dontTouch(expend_ready)
  expend_ready := Cat(io.readys.asUInt, io.readys.asUInt)

  val shifted_ready = PriorityEncoderOH(expend_ready >> rrcnt) << rrcnt; chisel3.dontTouch(shifted_ready)

  val big_rrcnt = OHToUInt(shifted_ready)
  rrcnt_nxt := Mux(big_rrcnt >= numThread.U, big_rrcnt - numThread.U, big_rrcnt)

  when(reset.asBool)
  { io.issues := 0.U }
  .otherwise
  { io.issues := (io.readys.orR).asUInt << rrcnt_nxt }

}

class FrontEnd(implicit p: Parameters) extends LazyModule with NpusParams 
{
  val masternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"FrontEnd",
                                                      id   = IdRange(0, 1 << 1))))))
  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val test = Output(UInt(8.W))
      val thread_readys = Input(Vec(numThread, Bool()))
      val redirect = Input(Bool())
      val redirect_thread = Input(UInt(log2Up(numThread+1).W))
      val redirect_npc = Input(UInt(32.W))
    })
    chisel3.dontTouch(io)
    val (out, edge) = masternode.out(0)

    //thread state
    val thread_s_reset :: thread_halt :: thread_s_stall :: thread_s_ready :: thread_s_running :: Nil = Enum(5)
    val thread_states = RegInit(VecInit(Seq.fill(numThread)(thread_s_reset)));thread_states.foreach(chisel3.dontTouch(_))
    for(i <- 0 until numThread) { when(io.thread_readys(i)) { thread_states(i) := thread_s_ready } }

    val readys = VecInit(Seq.tabulate(numThread) { i => thread_states(i) === thread_s_ready } ).asUInt
    val rrsch = Module(new RRScheduler)
    rrsch.io.readys := readys

    val fetch_data = RegInit(0.U);chisel3.dontTouch(fetch_data)
    val thread_npc = RegInit(VecInit(Seq.fill(numThread)(reset_vector.asUInt(32.W))));thread_npc.foreach(chisel3.dontTouch(_))
    when(io.redirect) { thread_npc(io.redirect_thread) := io.redirect_npc }
    val req_pc = PriorityMux(rrsch.io.issues, thread_npc)

    chisel3.dontTouch(out.ar)
    chisel3.dontTouch(out.r)
    out.r.ready := true.B
    out.ar.valid := false.B
    out.ar.bits.id := 1.U
    //frontend FSM state
    val front_s_reset :: front_s_req :: front_s_resp :: front_s_ecc :: front_s_scan :: front_s_full :: Nil = Enum(6)
    val front_state = RegInit(front_s_reset);chisel3.dontTouch(front_state)
    switch(front_state) {
      is(front_s_reset) { front_state := front_s_req }
      is(front_s_req) { 
        out.ar.valid := true.B
        out.ar.bits.addr := req_pc
        when(out.ar.fire()) { front_state := front_s_resp } 
      }
      is(front_s_resp) { 
        when(out.r.fire()) { 
          front_state := front_s_ecc 
          fetch_data := out.r.bits.data
        } 
      }
      is(front_s_ecc) { front_state := front_s_scan }
      is(front_s_scan) {  }
      is(front_s_full) {  }
    }
  }
}







