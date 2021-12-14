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

  val rrcnt = RegInit(0.U((tidWidth+1).W))
  val rrcnt_nxt = Wire(UInt((tidWidth+1).W))
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
      val redirect_thread = Input(UInt(tidWidth.W))
      val redirect_npc = Input(UInt(pcWidth.W))
      val core = Output(Valid( new Bundle {
          val tid = UInt(tidWidth.W)
          val instr = UInt(instrWidth.W) }
      ))
    })
    chisel3.dontTouch(io)
    val (out, edge) = masternode.out(0)

    //thread state
    val thread_s_reset :: thread_halt :: thread_s_stall :: thread_s_ready :: thread_s_running :: Nil = Enum(5)
    val thread_states_R = RegInit(VecInit(Seq.fill(numThread)(thread_s_reset)));thread_states_R.foreach(chisel3.dontTouch(_))
    for(i <- 0 until numThread) { when(io.thread_readys(i)) { thread_states_R(i) := thread_s_ready } }

    //val readys = VecInit(Seq.tabulate(numThread) { i => thread_states_R(i) === thread_s_ready } ).asUInt
    val readys = 1.U //to do
    val rrsch = Module(new RRScheduler);chisel3.dontTouch(rrsch.io)
    rrsch.io.readys := readys

    val fetch_data_R = RegInit(0.U);chisel3.dontTouch(fetch_data_R)
    val fetch_tid_R = RegInit(0.U);chisel3.dontTouch(fetch_tid_R)

    val thread_npc_R = RegInit(VecInit(Seq.fill(numThread)(reset_vector.asUInt(pcWidth.W))));thread_npc_R.foreach(chisel3.dontTouch(_))
    when(io.redirect) { thread_npc_R(io.redirect_thread) := io.redirect_npc }

    val fetch_s_req_pc_R = RegInit(0.U(pcWidth.W))
    val fetch_s_req_tid_R = RegInit(0.U(tidWidth.W))
    val halting = WireInit(false.B)

    chisel3.dontTouch(out.ar)
    chisel3.dontTouch(out.r)
    out.r.ready := true.B
    out.ar.valid := false.B
    out.ar.bits.id := 1.U

    val instr_cnt_R = RegInit(0.U(log2Up(2*fetchInstrs + 1).W));chisel3.dontTouch(instr_cnt_R)
    val tid_buff_R = RegInit(0.U((2*fetchInstrs*tidWidth).W))
    val instr_buff_R = RegInit(0.U((2*fetchInstrs*instrWidth).W))
    when(instr_cnt_R =/= 0.U)
    {
      instr_cnt_R := instr_cnt_R - 1.U
      instr_buff_R := instr_buff_R >> instrWidth
      tid_buff_R := tid_buff_R >> tidWidth
    }
    io.core.valid := instr_cnt_R.orR
    io.core.bits.tid := tid_buff_R(tidWidth - 1, 0)
    io.core.bits.instr := instr_buff_R(instrWidth - 1, 0)

    //frontend FSM state
    val fetch_s_reset :: fetch_s_req :: fetch_s_resp :: fetch_s_ecc :: fetch_s_scan :: fetch_s_nospace :: Nil = Enum(6)
    val fetch_state_R = RegInit(fetch_s_reset);chisel3.dontTouch(fetch_state_R)
    switch(fetch_state_R) 
    {
      is(fetch_s_reset) { fetch_state_R := fetch_s_req }
      is(fetch_s_req) 
      { 
        //make instr reqest msg
        out.ar.valid := true.B
        out.ar.bits.addr := PriorityMux(rrsch.io.issues, thread_npc_R)
        fetch_s_req_pc_R := PriorityMux(rrsch.io.issues, thread_npc_R)
        fetch_s_req_tid_R := OHToUInt(rrsch.io.issues)
        when(out.ar.fire()) { fetch_state_R := fetch_s_resp } 

        //update npc
        for(i <- 0 until numThread) 
        { 
          when(out.ar.fire() && i.U === OHToUInt(rrsch.io.issues)) 
          { thread_npc_R(i) := ((thread_npc_R(i) >> fetchAddrOffWidth) + 1.U ) << fetchAddrOffWidth } 
        }
      }
      is(fetch_s_resp) 
      { 
        when(out.r.fire()) 
        { 
          //store instr data to register and shiftout unwanted data
          fetch_tid_R := Fill(fetchInstrs, fetch_s_req_tid_R) >> (fetch_s_req_pc_R(fetchAddrOffWidth-1 ,0) << tidWidth)
          fetch_data_R := out.r.bits.data >> (fetch_s_req_pc_R(fetchAddrOffWidth-1 ,0) << fetchAddrOffWidth)
          fetch_state_R := fetch_s_ecc
        } 
      }
      is(fetch_s_ecc) { fetch_state_R := fetch_s_scan }
      is(fetch_s_scan) 
      { 
        when(instr_cnt_R <= (instr_buff_R.getWidth/instrWidth - fetchInstrs + 1).U) 
        {
          //putinto instr buff
          instr_buff_R := Mux(instr_cnt_R === 0.U, fetch_data_R,  
            Cat(0.U(instrWidth.W), instr_buff_R(2*fetchInstrs*instrWidth-1, instrWidth)) 
              | (fetch_data_R << ((instr_cnt_R-1.U) << log2Up(instrWidth)) ) )

          tid_buff_R := Mux(instr_cnt_R === 0.U, fetch_tid_R,  
            Cat(0.U(tidWidth.W), tid_buff_R(2*fetchInstrs*tidWidth-1, tidWidth)) 
              | ( fetch_tid_R << ((instr_cnt_R-1.U) << log2Up(tidWidth)) ) )

          val instr_num = (fetchBytes.U - fetch_s_req_pc_R(fetchAddrOffWidth-1 ,0)) >> instrAddrOffWidth
          //update instr cnt
          instr_cnt_R := Mux(instr_cnt_R === 0.U, instr_num, instr_num + instr_cnt_R - 1.U)
          fetch_state_R := fetch_s_req 
          //check redirect instr
          val redirects = WireInit(VecInit(Seq.fill(fetchInstrs)(false.B))) //to do by dongdeji
          when(redirects.asUInt.orR) { halting := true.B }
        }
        .otherwise { fetch_state_R := fetch_s_nospace }
      }
      is(fetch_s_nospace) 
      { //TBD by dongdeji
        when(instr_cnt_R <= (instr_buff_R.getWidth/instrWidth - fetchInstrs + 1).U) 
        {
          //putinto instr buff
          instr_buff_R := Mux(instr_cnt_R === 0.U, fetch_data_R,  
            Cat(0.U(instrWidth.W), instr_buff_R(2*fetchBytes*8-1, instrWidth)) & fetch_data_R << ((instr_cnt_R-1.U) << instrAddrOffWidth) )
          val instr_num = (fetchBytes.U - fetch_s_req_pc_R(fetchAddrOffWidth-1 ,0)) >> instrAddrOffWidth
          //update instr cnt
          instr_cnt_R := Mux(instr_cnt_R === 0.U, instr_num, instr_num + instr_cnt_R - 1.U)
          fetch_state_R := fetch_s_req 
        }
        .otherwise { fetch_state_R := fetch_s_req }
      }
    }  //end of switch(fetch_state_R)

  }
}







