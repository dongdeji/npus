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
  val inst = Output(Valid( new Bundle {
      val halt_last = Bool()
      val tid = UInt(tidWidth.W)
      val pc = UInt(addrWidth.W)
      val inst = UInt(instWidth.W) } ))
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
    val thread_s_halt :: thread_s_ready :: Nil = Enum(2)
    val thread_states_R = RegInit(VecInit(Seq.fill(numThread)(thread_s_ready)));thread_states_R.foreach(chisel3.dontTouch(_))
    for(tid <- 0 until numThread) 
    { 
      assert(Cat((io.core.readys.valid && io.core.readys.bits.thread(tid)).asUInt, (halting && (fetch_s_req_tid_R === tid.U)).asUInt) =/= 3.U)
      when((io.core.readys.valid && io.core.readys.bits.thread(tid)) || (halting && (fetch_s_req_tid_R === tid.U))) 
      { 
        thread_states_R(tid) := 
          Mux(io.core.readys.valid && io.core.readys.bits.thread(tid), thread_s_ready, thread_s_halt) 
      } 
    }

    val readys = VecInit(Seq.tabulate(numThread) { tid => thread_states_R(tid) === thread_s_ready } ).asUInt
    val rrsch = Module(new RRScheduler);chisel3.dontTouch(rrsch.io)
    rrsch.io.readys := readys

    val fetch_last_R = RegInit(0.U);chisel3.dontTouch(fetch_last_R)
    val fetch_tids_R = RegInit(0.U);chisel3.dontTouch(fetch_tids_R)
    val fetch_pcs_R = RegInit(0.U);chisel3.dontTouch(fetch_pcs_R)
    val fetch_data_R = RegInit(0.U);chisel3.dontTouch(fetch_data_R)
    
    val reset_vector = (iramGlobalBase + iramSizePerCluster*ClusterId).asUInt(addrWidth.W)
    val thread_npc_R = RegInit(VecInit(Seq.fill(numThread)(reset_vector)));
    thread_npc_R.foreach(chisel3.dontTouch(_))
    //when(io.core.redirect.valid) { thread_npc_R(io.core.redirect.bits.tid) := io.core.redirect.bits.npc } move to bottom for high priority

    chisel3.dontTouch(out.ar)
    chisel3.dontTouch(out.r)
    out.r.ready := true.B
    out.ar.valid := false.B
    out.ar.bits.id := 1.U

    val inst_cnt_R = RegInit(0.U(log2Ceil(2*fetchInstrs + 1).W));chisel3.dontTouch(inst_cnt_R)
    val halt_last_buff_R = RegInit(0.U((2*fetchInstrs*1).W))
    val tid_buff_R = RegInit(0.U((2*fetchInstrs*tidWidth).W))
    val pc_buff_R = RegInit(0.U((2*fetchInstrs*addrWidth).W))
    val inst_buff_R = RegInit(0.U((2*fetchInstrs*instWidth).W))
    when(inst_cnt_R =/= 0.U)
    {
      inst_cnt_R := inst_cnt_R - 1.U
      halt_last_buff_R := halt_last_buff_R >> 1
      tid_buff_R := tid_buff_R >> tidWidth
      pc_buff_R := pc_buff_R >> addrWidth
      inst_buff_R := inst_buff_R >> instWidth
    }
    io.core.inst.valid := inst_cnt_R.orR    
    io.core.inst.bits.halt_last := halt_last_buff_R(0).asBool
    io.core.inst.bits.tid := tid_buff_R(tidWidth - 1, 0)
    io.core.inst.bits.pc := pc_buff_R(addrWidth - 1, 0)
    io.core.inst.bits.inst := inst_buff_R(instWidth - 1, 0)

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
        //make inst reqest msg
        out.ar.valid := rrsch.io.issue_OH.orR
        out.ar.bits.addr := PriorityMux(rrsch.io.issue_OH, thread_npc_R)
        fetch_s_req_pc_R := PriorityMux(rrsch.io.issue_OH, thread_npc_R)
        fetch_s_req_tid_R := rrsch.io.issue_tid
        when(out.ar.fire()) { fetch_state_R := fetch_s_resp } 

        //update npc
        for(tid <- 0 until numThread) 
        { 
          when(out.ar.fire() && tid.U === rrsch.io.issue_tid) 
          { thread_npc_R(tid) := ((thread_npc_R(tid) >> log2Ceil(fetchBytes)) + 1.U ) << log2Ceil(fetchBytes) } 
        }
      }
      is(fetch_s_resp) 
      { 
        when(out.r.fire()) 
        { 
          //store inst data to register and shiftout unwanted data
          fetch_last_R := Cat(1.U(1.W), 0.U((fetchInstrs-1).W)) >> fetch_s_req_pc_R(log2Ceil(fetchBytes)-1 ,log2Ceil(instBytes))
          fetch_tids_R := Fill(fetchInstrs, fetch_s_req_tid_R) >> (fetch_s_req_pc_R(log2Ceil(fetchBytes)-1 ,0) << log2Ceil(8))
          val fetch_pcs_raw = VecInit(Seq.tabulate(fetchInstrs){ i => 
                           ((fetch_s_req_pc_R >> log2Ceil(fetchBytes)) << log2Ceil(fetchBytes)) + (i*instBytes).U }).asUInt
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
        when(inst_cnt_R <= (inst_buff_R.getWidth/instWidth - fetchInstrs + 1).U) 
        {
            {// this part is the same with fetch_s_nospace's corresponding part
            //putinto inst buff
            inst_buff_R := Mux(inst_cnt_R === 0.U, fetch_data_R,  
              Cat(0.U(instWidth.W), inst_buff_R(2*fetchInstrs*instWidth-1, instWidth)) 
                | (fetch_data_R << ((inst_cnt_R-1.U) << log2Ceil(instWidth)) ) )
            
            val fetch_halt_last = Fill(fetchInstrs, halting) & fetch_last_R
            halt_last_buff_R := Mux(inst_cnt_R === 0.U, fetch_halt_last,  
              Cat(0.U(1.W), halt_last_buff_R(2*fetchInstrs*1-1, 1)) 
                | ( fetch_halt_last << ((inst_cnt_R-1.U) << log2Ceil(1)) ) )

            tid_buff_R := Mux(inst_cnt_R === 0.U, fetch_tids_R,  
              Cat(0.U(tidWidth.W), tid_buff_R(2*fetchInstrs*tidWidth-1, tidWidth)) 
                | ( fetch_tids_R << ((inst_cnt_R-1.U) << log2Ceil(tidWidth)) ) )

            pc_buff_R := Mux(inst_cnt_R === 0.U, fetch_pcs_R,  
              Cat(0.U(addrWidth.W), pc_buff_R(2*fetchInstrs*addrWidth-1, addrWidth)) 
                | ( fetch_pcs_R << ((inst_cnt_R-1.U) << log2Ceil(addrWidth)) ) )

            val fetch_inst_num = (fetchBytes.U - fetch_s_req_pc_R(log2Ceil(fetchBytes)-1 ,0)) >> log2Ceil(instBytes)
            //update inst cnt
            inst_cnt_R := Mux(inst_cnt_R === 0.U, fetch_inst_num, fetch_inst_num + inst_cnt_R - 1.U)
          }

          fetch_state_R := fetch_s_req 
          //check redirect inst
          val inst_mask = Fill(fetchInstrs, true.B) >> fetch_s_req_pc_R(log2Ceil(fetchBytes)-1 ,log2Ceil(instBytes))
          val inst_redirects = WireInit(VecInit(Seq.tabulate(fetchInstrs){ i => haltCondition(fetch_data_R((i+1)*instWidth - 1, i*instWidth)) }))
          val isRedirects = inst_mask & inst_redirects.asUInt
          debug1 := inst_mask
          debug2 := inst_redirects.asUInt
          when(isRedirects.asUInt.orR) { halting := true.B }
        }
        .otherwise { fetch_state_R := fetch_s_nospace }
      }
      is(fetch_s_nospace) // to do by dongdeji here has bug
      {
        when(inst_cnt_R <= (inst_buff_R.getWidth/instWidth - fetchInstrs + 1).U) 
        {
          {// this part is the same with fetch_s_scan's corresponding part
            //putinto inst buff
            inst_buff_R := Mux(inst_cnt_R === 0.U, fetch_data_R,  
              Cat(0.U(instWidth.W), inst_buff_R(2*fetchInstrs*instWidth-1, instWidth)) 
                | (fetch_data_R << ((inst_cnt_R-1.U) << log2Ceil(instWidth)) ) )

            val fetch_halt_last = Fill(fetchInstrs, thread_states_R(fetch_s_req_tid_R) === thread_s_halt) & fetch_last_R
            halt_last_buff_R := Mux(inst_cnt_R === 0.U, fetch_halt_last,  
              Cat(0.U(1.W), halt_last_buff_R(2*fetchInstrs*1-1, 1)) 
                | ( fetch_halt_last << ((inst_cnt_R-1.U) << log2Ceil(1)) ) )

            tid_buff_R := Mux(inst_cnt_R === 0.U, fetch_tids_R,  
              Cat(0.U(tidWidth.W), tid_buff_R(2*fetchInstrs*tidWidth-1, tidWidth)) 
                | ( fetch_tids_R << ((inst_cnt_R-1.U) << log2Ceil(tidWidth)) ) )

            pc_buff_R := Mux(inst_cnt_R === 0.U, fetch_pcs_R,  
              Cat(0.U(addrWidth.W), pc_buff_R(2*fetchInstrs*addrWidth-1, addrWidth)) 
                | ( fetch_pcs_R << ((inst_cnt_R-1.U) << log2Ceil(addrWidth)) ) )

            //val fetch_inst_num = (fetchBytes.U - fetch_s_req_pc_R(log2Ceil(fetchBytes)-1 ,0)) >> log2Ceil(instBytes)
            //update inst cnt
            //inst_cnt_R := Mux(inst_cnt_R === 0.U, fetch_inst_num, fetch_inst_num + inst_cnt_R - 1.U)
          }
          fetch_state_R := fetch_s_req 
        }
        .otherwise { fetch_state_R := fetch_s_nospace }
      }
    }  //end of switch(fetch_state_R)

    // move to here to make this has high priority    
    when(io.core.redirect.valid) { thread_npc_R(io.core.redirect.bits.tid) := io.core.redirect.bits.npc }

  }// end of lazy val module = new LazyModuleImp(this)
}







