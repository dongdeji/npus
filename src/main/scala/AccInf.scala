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

class AccReqMetaBundle extends Bundle with NpusParams 
{
  val uop = new ThreadUop
  val req = new DmemReqBundle

  override def cloneType: this.type = (new AccReqMetaBundle).asInstanceOf[this.type]
}

class AccInf(ClusterId:Int, GroupId:Int, NpId: Int)(implicit p: Parameters) extends LazyModule with NpusParams 
{
  val regxbar = LazyModule(new AXI4Xbar)
  val iramxbar = LazyModule(new AXI4Xbar)
  val accxbar = LazyModule(new AXI4Xbar)

  private val irammasters = Seq.tabulate(numThread) 
  { tid => 
    val irammaster = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"irammaster$tid",
                                                      id = IdRange(0, numThread))))))
    iramxbar.node := irammaster
    irammaster
  }
  private val accmasters = Seq.tabulate(numThread) 
  { tid => 
    val accmaster = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"accmaster$tid",
                                                      id = IdRange(0, numThread))))))
    accxbar.node := accmaster
    accmaster
  }  
  private val regmasters = Seq.tabulate(numThread) 
  { tid => 
    val regmaster = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"regmaster$tid",
                                                      id = IdRange(0, numThread))))))
    regxbar.node := regmaster
    regmaster
  }

  lazy val module = new LazyModuleImp(this) 
  {
    val io = IO(new Bundle {
      val core = Flipped(new AccInfBundle)
    })

    val iramouts = irammasters.map { _.out(0)._1 }
    val accouts  = accmasters.map { _.out(0)._1 }
    val regouts  = regmasters.map { _.out(0)._1 }

    val iramaddress = AddressSet(iramBase + iramSizePerCluster*ClusterId, iramSizePerCluster-1)
    val dramaddress = AddressSet(dramBase + dramSizePerNp*(ClusterId*numGroup*numNpu + GroupId*numNpu + NpId), dramSizePerNp-1)

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
    /***************** handle dmem req end *****************/

    // handle acc/mmio/iram req
    /***************** handle acc/mmio/iram req begin *****************/
    val accReqMeta = RegInit(0.U.asTypeOf(Vec(numThread, new AccReqMetaBundle)))
    Seq.tabulate(numThread)
    { tid => 
      // remember the request meta
      when(io.core.req.valid && (tid.U === io.core.req.bits.tid)) 
      { 
        accReqMeta(tid).req := io.core.req.bits
        accReqMeta(tid).uop := io.core.uop
      } 

      when(iramaddress.contains(io.core.req.bits.addr))
      { // handle iram read req
        // to do by dongdeji
      }
    }

    /***************** handle acc/mmio/iram req end *****************/

  }
}







