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


class DmemReqBundle extends Bundle with NpusParams {  
  val cmd = UInt(M_SZ.W) /* dmem_req.ctrl.mem_cmd */
  val size = UInt(2.W) /* dmem_req.inst_32(13,12) */
  val signed = UInt(1.W) /* !dmem_req.inst_32(14) */
  val data = UInt(dataWidth.W)
  val addr = UInt(addrWidth.W)
  val tid = UInt(log2Up(numThread).W)

  override def cloneType: this.type = (new DmemReqBundle).asInstanceOf[this.type]
}

class DmemRespBundle extends Bundle with NpusParams {  
  val data = UInt(dataWidth.W)
  val addr = UInt(addrWidth.W)
  val tid = UInt(log2Up(numThread).W)

  override def cloneType: this.type = (new DmemRespBundle).asInstanceOf[this.type]
}

class AccInfBundle extends Bundle with NpusParams 
{
  val uop  = Output( new ThreadUop )
  val req  = Valid( new DmemReqBundle )
  val resp = Flipped(Valid( new DmemRespBundle ))
  val readys = Input(Valid(new Bundle { val thread = UInt(numThread.W) } ))

  override def cloneType: this.type = (new AccInfBundle).asInstanceOf[this.type]
}

class AccMetaBundle extends Bundle with NpusParams 
{
  val valid = Bool()
  val uop = new ThreadUop
  val req = new DmemReqBundle
  val reqed = Bool()
  val resped = Bool()
  val buff = UInt(dataWidth.W)
  val buff_full = Bool()

  override def cloneType: this.type = (new AccMetaBundle).asInstanceOf[this.type]
}

class AccInf(ClusterId:Int, GroupId:Int, NpId: Int)(implicit p: Parameters) extends LazyModule with NpusParams 
{
  val regxbar = LazyModule(new AXI4Xbar)
  val mmioxbar = LazyModule(new AXI4Xbar)
  val accxbar = LazyModule(new AXI4Xbar)

  private val mmiomasters = Seq.tabulate(numThread) 
  { tid => 
    val mmiomaster = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"mmiomaster$tid",
                                                      id = IdRange(0, 1),
                                                      maxFlight = Some(0))))))
    mmioxbar.node := mmiomaster
    mmiomaster
  }
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
    })

    val readys_thread = VecInit(Fill(numThread, false.B))
    chisel3.dontTouch(readys_thread)

    val Id = ClusterId*numGroup*numNpu + GroupId*numNpu + NpId
    val dramaddress = AddressSet(dramGlobalBase + dramSizePerNp*Id, dramSizePerNp-1)
    val iramaddress = AddressSet(iramGlobalBase + iramSizePerCluster*ClusterId, iramSizePerCluster-1)

    /***************** handle dmem req begin *****************/
    val req_valid = RegNext(io.core.req.valid)
    val req_cmd = RegNext(io.core.req.bits.cmd)
    val req_addr = RegNext(io.core.req.bits.addr)
    val req_tid = RegNext(io.core.req.bits.tid)
    val dramDepth = dramSizePerNp/dataBytes
    val banks = (0 until dataBytes ).map{ i => SyncReadMem(dramDepth, UInt(8.W)) }

    val wdata = Wire(Vec(dataBytes, UInt(8.W))); chisel3.dontTouch(wdata)
    wdata := (new StoreGen(io.core.req.bits.size, 0.U, io.core.req.bits.data, 8).data).asTypeOf(Vec(dataBytes, UInt(8.W)))

    val dsize = WireInit(1.U << io.core.req.bits.size); chisel3.dontTouch(dsize)
    val addr_h = Cat(io.core.req.bits.tid, io.core.req.bits.addr(log2Ceil(dramDepth) - log2Ceil(numThread) - 1, log2Ceil(dataBytes)))
    val addr_l = io.core.req.bits.addr(log2Ceil(dataBytes)-1, 0)
    val unmask_l = WireInit((-1.S(dataBytes.W) >> addr_l) << addr_l); chisel3.dontTouch(unmask_l)
    val unmask_h = WireInit((-1.S(dataBytes.W) >> (addr_l + dsize)) << (addr_l + dsize)); chisel3.dontTouch(unmask_h)
    val dmask = WireInit((~unmask_h & unmask_l)(dataBytes -1, 0)); chisel3.dontTouch(dmask)
    val enmask = WireInit(Fill(dataBytes, io.core.req.valid && io.core.req.bits.cmd.isOneOf(M_XRD, M_XWR)) & dmask); chisel3.dontTouch(enmask)

    Seq.tabulate(dataBytes){ i =>
      when(enmask(i) && io.core.req.bits.cmd.isOneOf(M_XWR))
      { banks(i).write(addr_h, wdata(i)) }
    }

    val enmask_s1 = RegNext(enmask)
    val rdatas = Seq.tabulate(dataBytes) { i => Mux(enmask_s1(i).asBool, banks(i).read(addr_h), 0.U) }
    val rdata = WireInit(Cat(rdatas.reverse))

    io.core.resp.bits.data := rdata >> (req_addr(log2Ceil(dataBytes)-1, 0) << log2Ceil(8))
    io.core.resp.valid := req_valid && dramaddress.contains(req_addr)
    io.core.resp.bits.addr := req_addr
    io.core.resp.bits.tid := req_tid 
    readys_thread(req_tid) := io.core.resp.valid
    /***************** handle dmem req end *****************/

    // handle acc/mmio/iram req    
    val mmioouts = mmiomasters.map { _.out(0)._1 }
    val regouts  = regmasters.map { _.out(0)._1 }    
    val mmioedgeOuts = mmiomasters.map { _.out(0)._2 }
    val regedgeOuts  = regmasters.map { _.out(0)._2 }

    /***************** handle acc/mmio/iram req begin *****************/
    val accMeta_R = RegInit(0.U.asTypeOf(Vec(numThread, new AccMetaBundle)))
    val debug1 = WireInit(0.U) ; chisel3.dontTouch(debug1)
    val debug2 = WireInit(0.U) ; chisel3.dontTouch(debug2)
    Seq.tabulate(numThread)
    { tid => 
      chisel3.dontTouch(mmioouts(tid))
      chisel3.dontTouch(regouts(tid))

      // remember the request meta
      when(io.core.req.valid && (tid.U === io.core.req.bits.tid)) 
      {         
        accMeta_R(tid).valid := true.B
        accMeta_R(tid).req := io.core.req.bits
        accMeta_R(tid).uop := io.core.uop
      }
      when(!accMeta_R(tid).reqed)
      {
        accMeta_R(tid).reqed := (mmioouts(tid).aw.fire() && mmioouts(tid).w.fire()) || mmioouts(tid).ar.fire()
      }
      accMeta_R(tid).resped := mmioouts(tid).b.fire() || mmioouts(tid).r.fire() 

      val slaves_address = mmioedgeOuts(tid).slave.slaves.map(_.address).flatten

      val ioReqValid = io.core.req.valid && slaves_address.map(_.contains(io.core.req.bits.addr)).orR
      val metaReqValid = accMeta_R(tid).valid && !accMeta_R(tid).reqed && 
                            slaves_address.map(_.contains(accMeta_R(tid).req.addr)).orR
      
      /************* handle read to rd process begin ************/
      mmioouts(tid).ar.valid := (ioReqValid && io.core.req.bits.cmd.isOneOf(M_XRD)) || 
                                (metaReqValid && accMeta_R(tid).req.cmd.isOneOf(M_XRD))
      mmioouts(tid).ar.bits.id := 0.U
      mmioouts(tid).ar.bits.addr := Mux(metaReqValid, accMeta_R(tid).req.addr, io.core.req.bits.addr)

      mmioouts(tid).r.ready := accMeta_R(tid).valid && (!accMeta_R(tid).buff_full)
      when(accMeta_R(tid).valid && mmioouts(tid).r.fire())
      { 
        accMeta_R(tid).buff := mmioouts(tid).r.bits.data >> (accMeta_R(tid).req.addr(log2Ceil(fetchBytes)-1, 0) << log2Ceil(8))
        accMeta_R(tid).buff_full := true.B
      }
      regouts(tid).aw.valid := accMeta_R(tid).valid && accMeta_R(tid).uop.ctrl.wxd && 
                                   accMeta_R(tid).buff_full
      val Id = ClusterId*numGroup*numNpu + GroupId*numNpu + NpId
      val npRegBase = (regfileGlobalBase + regfileSizePerNp*Id).U
      val threadRegAddr = npRegBase >> (log2Ceil(isaRegNumPerThread) + log2Ceil(dataBytes))
      val threadRegOff  = Cat(accMeta_R(tid).uop.rd, 0.U(log2Ceil(dataBytes).W))
      regouts(tid).aw.bits.addr := Cat(threadRegAddr, threadRegOff)
      regouts(tid).w.valid := regouts(tid).aw.valid
      regouts(tid).w.bits.data := accMeta_R(tid).buff
      when(regouts(tid).aw.fire() && regouts(tid).w.fire())
      { accMeta_R(tid) := 0.U.asTypeOf(new AccMetaBundle) }

      regouts(tid).b.ready := true.B
      when(regouts(tid).b.fire()) { readys_thread(tid) := true.B }
      /************* handle read to rd process end ************/

      /************* handle write out process begin ************/
      mmioouts(tid).aw.valid := (ioReqValid && io.core.req.bits.cmd.isOneOf(M_XWR)) || 
                                (metaReqValid && accMeta_R(tid).req.cmd.isOneOf(M_XWR))
      mmioouts(tid).aw.bits.addr := Mux(metaReqValid, accMeta_R(tid).req.addr, io.core.req.bits.addr)
      mmioouts(tid).w.valid := mmioouts(tid).aw.valid
      mmioouts(tid).w.bits.data := Mux(metaReqValid, accMeta_R(tid).req.data, io.core.req.bits.data)
      when(mmioouts(tid).aw.fire() && mmioouts(tid).w.fire())
      { 
        accMeta_R(tid) := 0.U.asTypeOf(new AccMetaBundle) 
        readys_thread(tid) := true.B
      }
      /************* handle write out process end ************/
    }
    /***************** handle acc/mmio/iram req end *****************/
    io.core.readys.valid := readys_thread.asUInt.orR
    io.core.readys.bits.thread := readys_thread.asUInt
  }
}







