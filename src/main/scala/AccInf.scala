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
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.tile._
import chisel3.experimental.chiselName


class StoreGen(typ: UInt, addr: UInt, dat: UInt, maxSize: Int = 8) 
{
  val size = typ(log2Ceil(log2Ceil(maxSize)+1)-1,0)

  def misaligned =
          (addr & ((1.U << size) - 1.U)(log2Ceil(maxSize)-1,0)).orR

  def mask = {
      var res = 1.U
      for (i <- 0 until log2Ceil(maxSize)) {
        val upper = Mux(addr(i), res, 0.U) | Mux(size >= (i+1).U, ((BigInt(1) << (1 << i))-1).U, 0.U)
        val lower = Mux(addr(i), 0.U, res)
        res = Cat(upper, lower)
      }
      res
  }
  protected def genData(i: Int): UInt =
            if (i >= log2Ceil(maxSize)) dat
            else Mux(size === i.U, Fill(1 << (log2Ceil(maxSize)-i), dat((8 << i)-1,0)), genData(i+1))

  def data = genData(0)
  def wordData = genData(2)
}

class LoadGen(typ: UInt, signed: Bool, addr: UInt, dat: UInt, zero: Bool, maxSize: Int) {
  private val size = new StoreGen(typ, addr, dat, maxSize).size

  private def genData(logMinSize: Int): UInt = {
    var res = dat
    for (i <- log2Up(maxSize)-1 to logMinSize by -1) {
      val pos = 8 << i
      val shifted = Mux(addr(i), res(2*pos-1,pos), res(pos-1,0))
      val doZero = (i == 0).B && zero
      val zeroed = Mux(doZero, 0.U, shifted)
      res = Cat(Mux(size === i.U || doZero, Fill(8*maxSize-pos, signed && zeroed(pos-1)), res(8*maxSize-1,pos)), zeroed)
    }
    res
  }

  def wordData = genData(2)
  def data = genData(0)
}


class DmemReqBundle extends Bundle with NpusParams {  
  val cmd = UInt(M_SZ.W) /* dmem_req.ctrl.mem_cmd */
  val size = UInt(2.W) /* dmem_req.inst_32(13,12) */
  val signed = UInt(1.W) /* !dmem_req.inst_32(14) */
  val data = UInt(dataWidth.W)
  val addr = UInt(addrWidth.W)
  val tid = UInt(log2Up(numThread).W)
  val uop = new ThreadUop

  override def cloneType: this.type = (new DmemReqBundle).asInstanceOf[this.type]
}

class KeyBufReqBundle extends Bundle with NpusParams {  
  val size = UInt(2.W) /* dmem_req.inst_32(13,12) */
  val data = UInt(dataWidth.W)
  val tid = UInt(log2Up(numThread).W)

  override def cloneType: this.type = (new KeyBufReqBundle).asInstanceOf[this.type]
}

class DmemRespBundle extends Bundle with NpusParams {  
  val data = UInt(dataWidth.W)
  val addr = UInt(addrWidth.W)
  val tid = UInt(log2Up(numThread).W)

  override def cloneType: this.type = (new DmemRespBundle).asInstanceOf[this.type]
}


class AccInfBundle extends Bundle with NpusParams 
{
  val req  = Valid( new DmemReqBundle )
  val resp = Flipped(Valid( new DmemRespBundle ))
  val readys = Input(Valid(new Bundle { val thread = UInt(numThread.W) } ))

  override def cloneType: this.type = (new AccInfBundle).asInstanceOf[this.type]
}

class AccMetaBundle extends Bundle with NpusParams 
{
  val valid = Bool()
  //val uop = new ThreadUop
  val req = new DmemReqBundle
  val reqed = Bool()
  val resped = Bool()
  val buff = UInt(dataWidth.W)
  val buff_full = Bool()
  val io_req = Bool()
  val queue_req = Bool()
  val keyOffset = UInt(log2Ceil(keyBuffSizePerNp).W)

  override def cloneType: this.type = (new AccMetaBundle).asInstanceOf[this.type]
}

class LoadPktReqBundle extends Bundle with NpusParams
{
  val tid = UInt(log2Up(numThread).W)
  val addr = UInt(addrWidth.W)
  override def cloneType: this.type = (new LoadPktReqBundle).asInstanceOf[this.type]
}
class AccLoadBundle extends Bundle with NpusParams
{
  val req = Decoupled(new LoadPktReqBundle)
  val readys = Flipped(Valid(new Bundle { val thread = UInt(numThread.W) } ))
  //val resp = Flipped(Valid(new Bundle {
  //      val tid = UInt(log2Up(numThread).W)
  //      val readys = UInt(numThread.W)
  //}))
}

class AccInf(ClusterId:Int, GroupId:Int, NpId: Int)(implicit p: Parameters) extends LazyModule with NpusParams 
{
  val regxbar = LazyModule(new AXI4Xbar)
  val accxbar = LazyModule(new AXI4Xbar)

  private val accmasters = Seq.tabulate(numThread) 
  { tid => 
    val accmaster = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"accmaster$tid",
                                                      id = IdRange(0, 1),
                                                      maxFlight = Some(0))))))
    accxbar.node := accmaster
    accmaster
  }  
  private val regmasters = Seq.tabulate(numThread) 
  { tid => 
    val regmaster = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"regmaster$tid",
                                                      id = IdRange(0, 1),
                                                      maxFlight = Some(0))))))
    regxbar.node := regmaster
    regmaster
  }

  lazy val module = new LazyModuleImp(this) 
  {
    val io = IO(new Bundle {
      val core = Flipped(new AccInfBundle)
      val accLoad = new AccLoadBundle  // to window
    })
    
    val accload = if(supportNpInstr) (io.core.req.bits.uop.ctrl.npi && (io.core.req.bits.uop.ctrl.npcmd === NpuCmd.NP_LWI)) else false.B
    io.accLoad.req.valid := io.core.req.valid && io.core.req.bits.uop.valid && io.core.req.bits.uop.ctrl.legal && accload
    io.accLoad.req.bits.tid := io.core.req.bits.tid
    io.accLoad.req.bits.addr := pktBuffBase.U

    io.core.resp.valid := false.B  // set resp defaut

    val readys_thread = VecInit(Fill(numThread, false.B))
    chisel3.dontTouch(readys_thread)

    // handle dmem access
    val dmem = Module(new Dmem(ClusterId, GroupId, NpId))
    dmem.io.core.req.bits.uop := io.core.req.bits.uop
    dmem.io.core.req := io.core.req
    when(dmem.io.core.resp.valid)
    { io.core.resp := dmem.io.core.resp }

    // handle keybuff access
    val keybuff = Module(new KeyBuff(ClusterId, GroupId, NpId))
    keybuff.io.core.valid := io.core.req.valid && io.core.req.bits.uop.valid && io.core.req.bits.uop.ctrl.legal && 
                               io.core.req.bits.uop.ctrl.npi && (io.core.req.bits.uop.ctrl.npcmd === NpuCmd.NP_STK)
    keybuff.io.core.bits.size := io.core.req.bits.size
    keybuff.io.core.bits.data := io.core.req.bits.data
    keybuff.io.core.bits.tid := io.core.req.bits.tid
    keybuff.io.read_offset := 0.U
    keybuff.io.read_tid := 0.U
    keybuff.io.reset_head := false.B
    keybuff.io.reset_tid := 0.U

    // handle acc/iram axi4 req    
    val accouts = accmasters.map { _.out(0)._1 }
    val regouts  = regmasters.map { _.out(0)._1 }    
    val accedgeOuts = accmasters.map { _.out(0)._2 }
    val regedgeOuts  = regmasters.map { _.out(0)._2 }

    val matchReqQ = Module(new Queue(io.core.req.bits.cloneType, numThread + 1, flow = true))
    chisel3.dontTouch(matchReqQ.io)
    matchReqQ.io.enq.valid := io.core.req.valid && io.core.req.bits.uop.ctrl.legal && 
                                io.core.req.bits.uop.ctrl.npi && 
                                (io.core.req.bits.uop.ctrl.npcmd === NpuCmd.NP_LKX)
    matchReqQ.io.enq.bits := io.core.req.bits
    matchReqQ.io.deq.ready := false.B

    /***************** handle acc/iram axi4 req begin *****************/
    val idle :: acc_rw_send :: wait_acc_r :: send_reg_aw :: wait_reg_b :: wait_acc_b :: start_readkey :: Nil = Enum(7)
    val state_R = RegInit(VecInit(Seq.fill(numThread)(idle))) ;state_R.foreach(chisel3.dontTouch(_))
    val accMeta_R = RegInit(0.U.asTypeOf(Vec(numThread, new AccMetaBundle)))
    accMeta_R.foreach(chisel3.dontTouch(_))
    val debug1 = WireInit(0.U) ; chisel3.dontTouch(debug1)
    val debug2 = WireInit(0.U) ; chisel3.dontTouch(debug2)
    Seq.tabulate(numThread)
    { tid => 
      chisel3.dontTouch(accouts(tid))
      chisel3.dontTouch(regouts(tid))

      val slaves_address = accedgeOuts(tid).slave.slaves.map(_.address).flatten
      
      // default connect
      accouts(tid).ar.valid := false.B
      accouts(tid).ar.bits.id := 0.U
      accouts(tid).r.ready := accMeta_R(tid).valid && (!accMeta_R(tid).buff_full)
      accouts(tid).aw.valid := false.B
      accouts(tid).aw.bits.id := 0.U
      accouts(tid).w.valid := false.B
      accouts(tid).w.bits.last := false.B
      accouts(tid).b.ready := true.B
      regouts(tid).ar.valid := false.B
      regouts(tid).ar.bits.id := 0.U
      regouts(tid).r.ready := true.B
      regouts(tid).aw.valid := false.B
      regouts(tid).aw.bits.id := 0.U
      regouts(tid).w.bits.last := true.B
      regouts(tid).w.valid := false.B
      regouts(tid).b.ready := true.B

      switch(state_R(tid)) 
      {
        is(idle) 
        {
          val io_req = io.core.req.valid && (tid.U === io.core.req.bits.tid) && 
                          slaves_address.map(_.contains(io.core.req.bits.addr)).orR &&
                            (io.core.req.bits.uop.ctrl.npcmd =/= NpuCmd.NP_LKX)
          val queue_req = matchReqQ.io.deq.valid && (tid.U === matchReqQ.io.deq.bits.tid)
          chisel3.dontTouch(io_req)
          chisel3.dontTouch(queue_req)
          assert( (io_req && queue_req) =/= true.B )
          when(io_req) 
          { // remember the request meta
            accMeta_R(tid) := 0.U.asTypeOf(new AccMetaBundle)
            accMeta_R(tid).valid := true.B
            accMeta_R(tid).req := io.core.req.bits            
            accMeta_R(tid).io_req := true.B
            state_R(tid) := acc_rw_send
          }
          when(queue_req) 
          { // remember the request meta            
            accMeta_R(tid) := 0.U.asTypeOf(new AccMetaBundle)
            accMeta_R(tid).valid := true.B            
            accMeta_R(tid).req := matchReqQ.io.deq.bits          
            accMeta_R(tid).queue_req := true.B         
            accMeta_R(tid).keyOffset := 0.U
            matchReqQ.io.deq.ready := true.B
            state_R(tid) := start_readkey
          }
        }
        is(start_readkey)
        {
          accMeta_R(tid).keyOffset := accMeta_R(tid).keyOffset + 1.U
          keybuff.io.read_offset := accMeta_R(tid).keyOffset
          keybuff.io.read_tid := accMeta_R(tid).req.tid
          state_R(tid) := acc_rw_send
        }
        is(acc_rw_send) 
        { 
          assert( (accMeta_R(tid).io_req && accMeta_R(tid).queue_req) =/= true.B )
          when(accMeta_R(tid).io_req)
          {
            accouts(tid).ar.valid := accMeta_R(tid).req.cmd.isOneOf(M_XRD) &&
                                      slaves_address.map(_.contains(accMeta_R(tid).req.addr)).orR
            accouts(tid).ar.bits.addr := accMeta_R(tid).req.addr
            accouts(tid).aw.valid := accMeta_R(tid).req.cmd.isOneOf(M_XWR) &&
                                      slaves_address.map(_.contains(accMeta_R(tid).req.addr)).orR
            accouts(tid).aw.bits.addr := accMeta_R(tid).req.addr
            accouts(tid).w.valid := accouts(tid).aw.valid
            accouts(tid).w.bits.data := accMeta_R(tid).req.data
            accouts(tid).w.bits.last := true.B
            
            when(accouts(tid).ar.fire()) 
            { state_R(tid) := wait_acc_r }

            when(accouts(tid).aw.fire() && accouts(tid).w.fire()) 
            { 
              accMeta_R(tid) := 0.U.asTypeOf(new AccMetaBundle) 
              readys_thread(tid) := true.B
              state_R(tid) := idle 
            }
          }
          when(accMeta_R(tid).queue_req)
          { // to do 
            accouts(tid).aw.valid := accMeta_R(tid).keyOffset === 1.U
            /*alu.io.in2 := MuxLookup(ex_uop_R.ctrl.sel_alu2, 0.S,
                                Seq(  A2_RS2 -> ex_uop_W.rs2_data.asSInt,
                                      A2_IMM -> ImmGen(ex_uop_R.ctrl.sel_imm, ex_uop_R.inst),
                                      A2_SIZE -> Mux(/*ex_uop_R.rvc*/false.B, 2.S, 4.S))).asUInt*/
            accouts(tid).aw.bits.addr := 0x54010000.U(addrWidth.W)
            accouts(tid).w.valid := true.B
            accouts(tid).w.bits.data := keybuff.io.read_data
            accouts(tid).w.bits.last := true.B // to do by dongdeji

            keybuff.io.reset_head := accouts(tid).w.bits.last
            keybuff.io.reset_tid := accMeta_R(tid).req.tid

            when(accouts(tid).w.fire())
            { accMeta_R(tid).keyOffset := accMeta_R(tid).keyOffset + 1.U }

            when(accouts(tid).w.fire() && accouts(tid).w.bits.last ) 
            { state_R(tid) := wait_acc_r }
          }
        }
        is(wait_acc_r)
        { 
          assert( (accMeta_R(tid).io_req && accMeta_R(tid).queue_req) =/= true.B )
          when(accMeta_R(tid).io_req)
          {
            when(accouts(tid).r.fire())
            { 
              val loadgen = new LoadGen(Cat(0.U(1.W), accMeta_R(tid).req.size), accMeta_R(tid).req.signed.asBool, 
                                        accMeta_R(tid).req.addr, accouts(tid).r.bits.data, false.B, fetchBytes)
              accMeta_R(tid).buff := loadgen.data
              accMeta_R(tid).buff_full := true.B              
              state_R(tid) := send_reg_aw
            }
          }
          when(accMeta_R(tid).queue_req)
          {
            when(accouts(tid).r.fire())
            { 
              accMeta_R(tid).buff := accouts(tid).r.bits.data
              accMeta_R(tid).buff_full := true.B              
              state_R(tid) := send_reg_aw
            }
          }
        }
        is(send_reg_aw)
        { 
          regouts(tid).aw.valid := accMeta_R(tid).valid && accMeta_R(tid).buff_full && 
                                   accMeta_R(tid).req.uop.ctrl.wxd && (accMeta_R(tid).req.uop.ctrl.wxdv === XdValid.XD_LLL)
          val Id = ClusterId*numGroup*numNpu + GroupId*numNpu + NpId
          val npRegBase = (regfileGlobalBase + regfileSizePerNp*Id).U
          val threadRegAddr = npRegBase >> (log2Ceil(isaRegNumPerThread) + log2Ceil(dataBytes))
          val threadRegOff  = Cat(accMeta_R(tid).req.uop.rd, 0.U(log2Ceil(dataBytes).W))
          regouts(tid).aw.bits.addr := Cat(threadRegAddr, threadRegOff)
          regouts(tid).w.valid := regouts(tid).aw.valid

          regouts(tid).w.bits.data := accMeta_R(tid).buff
          when(regouts(tid).aw.fire() && regouts(tid).w.fire())
          { state_R(tid) := wait_reg_b }
        }
        is(wait_reg_b)
        { 
          when(regouts(tid).b.fire()) 
          { 
            accMeta_R(tid) := 0.U.asTypeOf(new AccMetaBundle) 
            readys_thread(tid) := true.B 
            state_R(tid) := idle
          }
        }
      } // end of switch(state_R(tid)) 
    } // end of Seq.tabulate(numThread)
    /***************** handle acc/iram axi4 req end *****************/
    io.core.readys.valid := readys_thread.asUInt.orR || dmem.io.core.readys.valid || io.accLoad.readys.valid
    io.core.readys.bits.thread := readys_thread.asUInt | dmem.io.core.readys.bits.thread | io.accLoad.readys.bits.thread
  }
}







