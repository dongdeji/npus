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
import freechips.rocketchip.util.{BundleMap}

class RegFiles(ClusterId:Int, GroupId:Int, NpId: Int)(implicit p: Parameters) extends LazyModule with NpusParams with NpusUtil
{
  val Id = ClusterId*numGroup*numNpu + GroupId*numNpu + NpId
  val address = AddressSet(0x5000000 + 0x400*Id, 0x3ff)
  val slavenode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(address),
      //resources     = resources,
      regionType    = if (true) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = true,
      supportsRead  = TransferSizes(1, dataBytes),
      supportsWrite = TransferSizes(1, dataBytes),
      interleavedId = Some(0))),
    beatBytes  = dataBytes,
    requestKeys = if (true) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  lazy val module = new LazyModuleImp(this) 
  {
    val io = IO(new Bundle {
      val rd_write = Input(Bool()                       )
      val rd_data  = Input(Bits(dataWidth.W)            )
      val rd       = Input(UInt((log2Up(numThread)+5).W))
      val rs1      = Input(UInt((log2Up(numThread)+5).W))
      val rs2      = Input(UInt((log2Up(numThread)+5).W))
      val rs1_data = Output(Bits(dataWidth.W)           )
      val rs2_data = Output(Bits(dataWidth.W)           )
    })
    
    val banks1 = Seq.tabulate(dataBytes) { i => SyncReadMem(numThread*32, UInt(8.W)) }
    val banks2 = Seq.tabulate(dataBytes) { i => SyncReadMem(numThread*32, UInt(8.W)) }
    
    // x0 is always 0
    io.rs1_data := Mux(io.rs1(4, 0) === 0.U, 0.U, VecInit(Seq.tabulate(dataBytes){ i => banks1(i).read(io.rs1)}).asUInt)
    io.rs2_data := Mux(io.rs2(4, 0) === 0.U, 0.U, VecInit(Seq.tabulate(dataBytes){ i => banks2(i).read(io.rs2)}).asUInt)

    when(io.rd_write)
    {
      Seq.tabulate(dataBytes){ i => banks1(i).write(io.rd, io.rd_data(i*8+7, i*8)) }
      Seq.tabulate(dataBytes){ i => banks2(i).write(io.rd, io.rd_data(i*8+7, i*8)) }
    }

    /********** handle axi4 write interface begin **********/
    val (in, edgeIn) = slavenode.in(0)
    chisel3.dontTouch(in)

    val w_addr = Cat((mask(address, dataBytes) zip (in.aw.bits.addr >> log2Ceil(dataBytes)).asBools).filter(_._1).map(_._2).reverse)
    val w_sel0 = address.contains(in.aw.bits.addr)

    val w_full = RegInit(false.B)
    val w_id   = Reg(UInt())
    val w_echo = Reg(BundleMap(in.params.echoFields))
    val w_sel1 = RegInit(false.B)

    when (in.aw.fire()) { w_full := true.B }

    when (in.aw.fire()) 
    {
      w_id := in.aw.bits.id
      w_sel1 := w_sel0
      w_echo :<= in.aw.bits.echo
    }

    val wdata = VecInit.tabulate(dataBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
    Seq.tabulate(dataBytes) 
    { i=> 
      when (in.aw.fire() && w_sel0 && in.w.bits.strb(i).asBool) 
      { 
        banks1(i).write(in.aw.bits.addr >> log2Ceil(dataBytes), wdata(i))         
        banks2(i).write(in.aw.bits.addr >> log2Ceil(dataBytes), wdata(i)) 
      }
    }

    in.aw.ready := (in. w.valid && (in.b.ready || !w_full)) && (!io.rd_write)
    in.w.ready  := (in.aw.valid && (in.b.ready || !w_full)) && (!io.rd_write)

    in.b.valid     := w_full
    in.b.bits.id   := w_id
    in.b.bits.resp := Mux(w_sel1, AXI4Parameters.RESP_OKAY, AXI4Parameters.RESP_DECERR)
    in.b.bits.echo :<= w_echo
    /********** handle axi4 write interface end **********/

  }
}







