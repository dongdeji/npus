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
      val thread_redirect = Input(Vec(numThread, Bool()))
      val thread_npc = Input(Vec(numThread, UInt(32.W)))
    })
    chisel3.dontTouch(io)
    val (out, edge) = masternode.out(0)

    //frontend FSM state
    val front_s_reset :: front_s_req :: front_s_resp :: front_s_ecc :: front_s_scan :: front_s_full :: Nil = Enum(6)
    //thread FSM state
    val thread_s_reset :: thread_halt :: thread_s_stall :: thread_s_ready :: thread_s_running :: Nil = Enum(5)

    val front_state = RegInit(front_s_reset);chisel3.dontTouch(front_state)
    val thread_states = RegInit(VecInit(Seq.fill(numThread)(thread_s_reset)));thread_states.foreach(chisel3.dontTouch(_))
    val thread_npc = RegInit(VecInit(Seq.fill(numThread)(reset_vector.asUInt)));thread_npc.foreach(chisel3.dontTouch(_))
  

  }
}







