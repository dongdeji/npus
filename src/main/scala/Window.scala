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
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{GenericLogicalTreeNode}
import freechips.rocketchip.util.{BundleMap}


class Window(ClusterId:Int, GroupId:Int, NpId: Int)(implicit p: Parameters) extends LazyModule with NpusParams
{
  val Id = ClusterId*numGroup*numNpu + GroupId*numNpu + NpId
  val address = AddressSet(windowGlobalBase + windowSizePerNp*Id, windowSizePerNp-1)
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
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
    val offsetWith = 1 << log2Ceil(log2Ceil(windowSizePerNp))
    val io = IO(new Bundle {
      val offset = Input(UInt(offsetWith.W))
      val size = Input(UInt(2.W)) /* dmem_req.inst_32(13,12) */
      val signed = Input(UInt(1.W)) /* !dmem_req.inst_32(14) */
      val data = Output(UInt(dataWidth.W))
    })
    chisel3.dontTouch(io)

    val banks = Seq.tabulate(dataBytes) { i => SyncReadMem(windowSizePerNp/dataBytes, UInt(8.W)) }
    
    val offset_raw = VecInit(Seq.tabulate(dataBytes){ i => (io.offset + i.U)(offsetWith-1, 0)}).asUInt
    chisel3.dontTouch(offset_raw)
    val byte_offset = io.offset(log2Ceil(dataBytes)-1, 0)
    val wide_offset_raw = offset_raw << (byte_offset << log2Ceil(offsetWith))
    chisel3.dontTouch(wide_offset_raw)
    val offset_pakage = (wide_offset_raw >> offsetWith*dataBytes) | wide_offset_raw(offsetWith*dataBytes-1,0)
    chisel3.dontTouch(offset_pakage)
    val byte_offset_s1 = RegNext(byte_offset)
    chisel3.dontTouch(byte_offset_s1)
    val size_s1 = RegNext(io.size)
    val signed_s1 = RegNext(io.signed)
    val data_pakage = VecInit(Seq.tabulate(dataBytes) { i => banks(i).read(offset_pakage((i+1)*offsetWith-1, i*offsetWith)) }).asUInt
    val data_raw_l = data_pakage >> (byte_offset_s1 << log2Ceil(8))
    val data_raw_h = (data_pakage << ((dataBytes.U - byte_offset_s1) << log2Ceil(8)))(dataWidth - 1, 0)
    val dataMask = ~( ((~(0.U(dataBytes.W))) << (1.U << size_s1)) (dataBytes-1,0) )
    chisel3.dontTouch(dataMask)
    io.data := (data_raw_h | data_raw_l) & FillInterleaved(8, dataMask)

    /********** handle axi4 write interface begin **********/
    val (in, edgeIn) = node.in(0)
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
      { banks(i).write(in.aw.bits.addr >> log2Ceil(dataBytes), wdata(i)) }
    }

    in.aw.ready := in. w.valid && (in.b.ready || !w_full)
    in.w.ready  := in.aw.valid && (in.b.ready || !w_full)

    in.b.valid     := w_full
    in.b.bits.id   := w_id
    in.b.bits.resp := Mux(w_sel1, AXI4Parameters.RESP_OKAY, AXI4Parameters.RESP_DECERR)
    in.b.bits.echo :<= w_echo
    /********** handle axi4 write interface end **********/

  }
}





