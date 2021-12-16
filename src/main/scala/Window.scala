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


class Window(ClusterId:Int, GroupId:Int, NpId: Int)(implicit p: Parameters) extends LazyModule with NpusParams 
{
  val Id = ClusterId*numGroup*numNpu + GroupId*numNpu + NpId
  val address = AddressSet(0x6000000 + 0x400*Id, 0x3ff)
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
    val offsetWith = 1 << log2Up(log2Up(windowBytes))
    val io = IO(new Bundle {
      val r_offset = Input(UInt(offsetWith.W))
      val r_data   = Output(UInt(dataWidth.W))
    })
    chisel3.dontTouch(io)

    val banks = Seq.tabulate(dataBytes) { i => SyncReadMem(windowBytes/dataBytes, UInt(8.W)) }
    
    val offset_raw = VecInit(Seq.tabulate(dataBytes){ i => (io.r_offset + i.U)(offsetWith-1, 0)}).asUInt
    chisel3.dontTouch(offset_raw)
    val wide_offset_raw = offset_raw << (io.r_offset(log2Up(dataBytes)-1, 0) << log2Up(offsetWith))
    chisel3.dontTouch(wide_offset_raw)
    val offset_raw_pakage = (wide_offset_raw >> offsetWith*dataBytes) | wide_offset_raw(offsetWith*dataBytes-1,0)
    chisel3.dontTouch(offset_raw_pakage)
    
    val data_raw = VecInit(Seq.tabulate(dataBytes) { i => banks(i).read(offset_raw_pakage((i+1)*offsetWith-1, i*offsetWith)) }).asUInt
    chisel3.dontTouch(data_raw)
 
    val (in, edgeIn) = slavenode.in(0)
    chisel3.dontTouch(in)
    in.aw.ready := true.B
    in.w.ready := true.B
    def bigBits(x: BigInt, tail: List[Boolean] = Nil): List[Boolean] =
        if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)
    def mask: List[Boolean] = bigBits(address.mask >> log2Ceil(dataBytes))

    val w_addr = Cat((mask zip (in.aw.bits.addr >> log2Ceil(dataBytes)).asBools).filter(_._1).map(_._2).reverse)
    val w_sel0 = address.contains(in.aw.bits.addr)

    val w_full = RegInit(false.B)
    val w_id   = Reg(UInt())
    //val w_echo = Reg(BundleMap(in.params.echoFields))
    val w_sel1 = RegInit(false.B)

    when (in.aw.fire()) { w_full := true.B }

    when (in.aw.fire()) 
    {
      w_id := in.aw.bits.id
      w_sel1 := w_sel0
      //w_echo :<= in.aw.bits.echo
    }

    val wdata = VecInit.tabulate(dataBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
    Seq.tabulate(dataBytes) 
    { i=> 
      when (in.aw.fire() && w_sel0 && in.w.bits.strb(i).asBool) 
      { banks(i).write(in.aw.bits.addr >> log2Ceil(dataBytes), wdata(i)) }
    }

    //io.r_data := VecInit(Seq.tabulate(dataBytes){ i => part1(i).read(io.rs1)})
  }
}





