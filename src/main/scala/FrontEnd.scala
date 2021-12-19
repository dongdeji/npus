/*
 to do
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
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.Instructions._

class RRScheduler extends Module with NpusParams
{
  val io = IO(new Bundle() {
                    val readys = Input(UInt(numThread.W))
                    val issue_OH = Output(UInt(numThread.W))
                    val issue_tid = Output(UInt(tidWidth.W))
                    })

  val issues_cnt = PopCount(io.issue_OH)
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
  { io.issue_OH := 0.U }
  .otherwise
  { io.issue_OH := (io.readys.orR).asUInt << rrcnt_nxt }
  
  io.issue_tid := OHToUInt(io.issue_OH)
}

class FrontEndBundle extends Bundle with NpusParams
{
  val instr = Output(Valid( new Bundle {
      val tid = UInt(tidWidth.W)
      val pc = UInt(addrWidth.W)
      val instr = UInt(instrWidth.W) } ))
  val redirect = Input(Valid( new Bundle {
      val tid = UInt(tidWidth.W)
      val npc = UInt(addrWidth.W) } ))
  val readys = Input(Valid(new Bundle { 
      val thread = UInt(numThread.W) } ))

  override def cloneType: this.type = (new FrontEndBundle).asInstanceOf[this.type]
}

class FrontEnd(ClusterId:Int, GroupId:Int, NpId: Int)(implicit p: Parameters) extends LazyModule with NpusParams 
{
  val masternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"FrontEnd",
                                                      id   = IdRange(0, 1 << 1))))))
  lazy val module = new LazyModuleImp(this) 
  {
    val io = IO(new Bundle {
      val core = new FrontEndBundle
    })
    chisel3.dontTouch(io)
    val (out, edge) = masternode.out(0)

    val fetch_s_req_pc_R = RegInit(0.U(addrWidth.W))
    val fetch_s_req_tid_R = RegInit(0.U(tidWidth.W))
    val halting = WireInit(false.B)

    //thread state
    val thread_s_reset :: thread_s_halt :: thread_s_stall :: thread_s_ready :: thread_s_running :: Nil = Enum(5)
    val thread_states_R = RegInit(VecInit(Seq.fill(numThread)(thread_s_ready)));thread_states_R.foreach(chisel3.dontTouch(_))
    for(i <- 0 until numThread) 
    { 
      assert(Cat((io.core.readys.valid && io.core.readys.bits.thread(i)).asUInt, (halting && (fetch_s_req_tid_R === i.U)).asUInt) =/= 3.U)
      when((io.core.readys.valid && io.core.readys.bits.thread(i)) || (halting && (fetch_s_req_tid_R === i.U))) 
      { thread_states_R(i) := Mux(io.core.readys.bits.thread(i), thread_s_ready, thread_s_halt) } 
    }

    val readys = VecInit(Seq.tabulate(numThread) { i => thread_states_R(i) === thread_s_ready } ).asUInt
    val rrsch = Module(new RRScheduler);chisel3.dontTouch(rrsch.io)
    rrsch.io.readys := readys

    val fetch_tids_R = RegInit(0.U);chisel3.dontTouch(fetch_tids_R)
    val fetch_pcs_R = RegInit(0.U);chisel3.dontTouch(fetch_pcs_R)
    val fetch_data_R = RegInit(0.U);chisel3.dontTouch(fetch_data_R)

    val thread_npc_R = RegInit(VecInit(Seq.fill(numThread)(reset_vector.asUInt(addrWidth.W))));thread_npc_R.foreach(chisel3.dontTouch(_))
    when(io.core.redirect.valid) { thread_npc_R(io.core.redirect.bits.tid) := io.core.redirect.bits.npc }

    chisel3.dontTouch(out.ar)
    chisel3.dontTouch(out.r)
    out.r.ready := true.B
    out.ar.valid := false.B
    out.ar.bits.id := 1.U

    val instr_cnt_R = RegInit(0.U(log2Ceil(2*fetchInstrs + 1).W));chisel3.dontTouch(instr_cnt_R)
    val tid_buff_R = RegInit(0.U((2*fetchInstrs*tidWidth).W))
    val pc_buff_R = RegInit(0.U((2*fetchInstrs*addrWidth).W))
    val instr_buff_R = RegInit(0.U((2*fetchInstrs*instrWidth).W))
    when(instr_cnt_R =/= 0.U)
    {
      instr_cnt_R := instr_cnt_R - 1.U
      tid_buff_R := tid_buff_R >> tidWidth
      pc_buff_R := pc_buff_R >> addrWidth
      instr_buff_R := instr_buff_R >> instrWidth
    }
    io.core.instr.valid := instr_cnt_R.orR
    io.core.instr.bits.tid := tid_buff_R(tidWidth - 1, 0)
    io.core.instr.bits.pc := pc_buff_R(addrWidth - 1, 0)
    io.core.instr.bits.instr := instr_buff_R(instrWidth - 1, 0)

    val debug1 = WireInit(0.U); chisel3.dontTouch(debug1)
    val debug2 = WireInit(0.U); chisel3.dontTouch(debug2)
    val debug3 = WireInit(0.U); chisel3.dontTouch(debug3)
    val debug4 = WireInit(0.U); chisel3.dontTouch(debug4)
    val debug5 = WireInit(0.U); chisel3.dontTouch(debug5)
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
        out.ar.bits.addr := PriorityMux(rrsch.io.issue_OH, thread_npc_R)
        fetch_s_req_pc_R := PriorityMux(rrsch.io.issue_OH, thread_npc_R)
        fetch_s_req_tid_R := rrsch.io.issue_tid
        when(out.ar.fire()) { fetch_state_R := fetch_s_resp } 

        //update npc
        for(i <- 0 until numThread) 
        { 
          when(out.ar.fire() && i.U === rrsch.io.issue_tid) 
          { thread_npc_R(i) := ((thread_npc_R(i) >> log2Ceil(fetchBytes)) + 1.U ) << log2Ceil(fetchBytes) } 
        }
      }
      is(fetch_s_resp) 
      { 
        when(out.r.fire()) 
        { 
          //store instr data to register and shiftout unwanted data
          fetch_tids_R := Fill(fetchInstrs, fetch_s_req_tid_R) >> (fetch_s_req_pc_R(log2Ceil(fetchBytes)-1 ,0) << log2Ceil(8))
          val fetch_pcs_raw = VecInit(Seq.tabulate(fetchInstrs){ i => 
                           ((fetch_s_req_pc_R >> log2Ceil(fetchBytes)) << log2Ceil(fetchBytes)) + (i*instrBytes).U }).asUInt
          fetch_pcs_R := fetch_pcs_raw >> (fetch_s_req_pc_R(log2Ceil(fetchBytes)-1 ,0) << log2Ceil(8))
          fetch_data_R := out.r.bits.data >> (fetch_s_req_pc_R(log2Ceil(fetchBytes)-1 ,0) << log2Ceil(8))
          fetch_state_R := fetch_s_ecc
        } 
      }
      is(fetch_s_ecc) 
      { 
        fetch_state_R := fetch_s_scan 
      }
      is(fetch_s_scan) 
      { 
        when(instr_cnt_R <= (instr_buff_R.getWidth/instrWidth - fetchInstrs + 1).U) 
        {
            {// this part is the same with fetch_s_nospace's corresponding part
            //putinto instr buff
            instr_buff_R := Mux(instr_cnt_R === 0.U, fetch_data_R,  
              Cat(0.U(instrWidth.W), instr_buff_R(2*fetchInstrs*instrWidth-1, instrWidth)) 
                | (fetch_data_R << ((instr_cnt_R-1.U) << log2Ceil(instrWidth)) ) )

            tid_buff_R := Mux(instr_cnt_R === 0.U, fetch_tids_R,  
              Cat(0.U(tidWidth.W), tid_buff_R(2*fetchInstrs*tidWidth-1, tidWidth)) 
                | ( fetch_tids_R << ((instr_cnt_R-1.U) << log2Ceil(tidWidth)) ) )

            pc_buff_R := Mux(instr_cnt_R === 0.U, fetch_pcs_R,  
              Cat(0.U(addrWidth.W), pc_buff_R(2*fetchInstrs*addrWidth-1, addrWidth)) 
                | ( fetch_pcs_R << ((instr_cnt_R-1.U) << log2Ceil(addrWidth)) ) )

            val fetch_instr_num = (fetchBytes.U - fetch_s_req_pc_R(log2Ceil(fetchBytes)-1 ,0)) >> log2Ceil(instrBytes)
            //update instr cnt
            instr_cnt_R := Mux(instr_cnt_R === 0.U, fetch_instr_num, fetch_instr_num + instr_cnt_R - 1.U)
          }

          fetch_state_R := fetch_s_req 
          //check redirect instr
          val instr_mask = Fill(fetchInstrs, true.B) >> fetch_s_req_pc_R(log2Ceil(fetchBytes)-1 ,log2Ceil(instrBytes))
          val instr_redirects = WireInit(VecInit(Seq.tabulate(fetchInstrs){ i => 
                fetch_data_R((i+1)*instrWidth - 1, i*instrWidth)(6,0).isOneOf(
                  Seq(BEQ.value.asUInt()(6,0), JAL.value.asUInt()(6,0), JALR.value.asUInt()(6,0))) })) //to do by dongdeji
          val isRedirects = instr_mask & instr_redirects.asUInt
          debug1 := instr_mask
          debug2 := instr_redirects.asUInt
          when(isRedirects.asUInt.orR) { halting := true.B }
        }
        .otherwise { fetch_state_R := fetch_s_nospace }
      }
      is(fetch_s_nospace) 
      {
        when(instr_cnt_R <= (instr_buff_R.getWidth/instrWidth - fetchInstrs + 1).U) 
        {
          {// this part is the same with fetch_s_scan's corresponding part
            //putinto instr buff
            instr_buff_R := Mux(instr_cnt_R === 0.U, fetch_data_R,  
              Cat(0.U(instrWidth.W), instr_buff_R(2*fetchInstrs*instrWidth-1, instrWidth)) 
                | (fetch_data_R << ((instr_cnt_R-1.U) << log2Ceil(instrWidth)) ) )

            tid_buff_R := Mux(instr_cnt_R === 0.U, fetch_tids_R,  
              Cat(0.U(tidWidth.W), tid_buff_R(2*fetchInstrs*tidWidth-1, tidWidth)) 
                | ( fetch_tids_R << ((instr_cnt_R-1.U) << log2Ceil(tidWidth)) ) )

            pc_buff_R := Mux(instr_cnt_R === 0.U, fetch_pcs_R,  
              Cat(0.U(addrWidth.W), pc_buff_R(2*fetchInstrs*addrWidth-1, addrWidth)) 
                | ( fetch_pcs_R << ((instr_cnt_R-1.U) << log2Ceil(addrWidth)) ) )

            val fetch_instr_num = (fetchBytes.U - fetch_s_req_pc_R(log2Ceil(fetchBytes)-1 ,0)) >> log2Ceil(instrBytes)
            //update instr cnt
            instr_cnt_R := Mux(instr_cnt_R === 0.U, fetch_instr_num, fetch_instr_num + instr_cnt_R - 1.U)
          }
          fetch_state_R := fetch_s_req 
        }
        .otherwise { fetch_state_R := fetch_s_req }
      }
    }  //end of switch(fetch_state_R)
  }// end of lazy val module = new LazyModuleImp(this)
}







