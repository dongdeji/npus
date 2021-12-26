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
  private val Id = ClusterId*numGroup*numNpu + GroupId*numNpu + NpId
  private val address = AddressSet(windowGlobalBase + windowSizePerNp*Id, windowSizePerNp-1)

  val masternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                    masters = Seq(AXI4MasterParameters(
                                                    name = s"windmaster$NpId",
                                                    id = IdRange(0, 1),
                                                    maxFlight = Some(0))))))

  lazy val module = new LazyModuleImp(this) 
  {
    val offsetWith = 1 << log2Ceil(log2Ceil(windowSizePerNp))
    val io = IO(new Bundle {
      val swap = Flipped(new SwapWindBundle)      
      val loadpkt = Flipped(new LoadPktWindBundle)
    })
    chisel3.dontTouch(io)

    val loadpktReqQ = Module(new Queue(io.loadpkt.req.bits.cloneType, numThread + 1))
    loadpktReqQ.io.enq.valid := io.loadpkt.req.valid
    loadpktReqQ.io.enq.bits := io.loadpkt.req.bits
    loadpktReqQ.io.deq.ready := false.B
    // to do by dongdeji

    val banks = Seq.tabulate(dataBytes) { i => SyncReadMem(windowSizePerNp/dataBytes, UInt(8.W)) }
    
    /********** handle swap req from pipe begin **********/
    val offset_raw = VecInit(Seq.tabulate(dataBytes){ i => (io.swap.offset + i.U)(offsetWith-1, 0)}).asUInt
    val byte_offset = io.swap.offset(log2Ceil(dataBytes)-1, 0)
    val wide_offset_raw = offset_raw << (byte_offset << log2Ceil(offsetWith))
    val offset_pakage = (wide_offset_raw >> offsetWith*dataBytes) | wide_offset_raw(offsetWith*dataBytes-1,0)
    val byte_offset_s1 = RegNext(byte_offset)
    val size_s1 = RegNext(io.swap.size)
    val signed_s1 = RegNext(io.swap.signed)
    val data_pakage = VecInit(Seq.tabulate(dataBytes) { i => banks(i).read(offset_pakage((i+1)*offsetWith-1, i*offsetWith)) }).asUInt
    val data_raw_l = data_pakage >> (byte_offset_s1 << log2Ceil(8))
    val data_raw_h = (data_pakage << ((dataBytes.U - byte_offset_s1) << log2Ceil(8)))(dataWidth - 1, 0)
    val dataMask = ~( ((~(0.U(dataBytes.W))) << (1.U << size_s1)) (dataBytes-1,0) )
    io.swap.data := (data_raw_h | data_raw_l) & FillInterleaved(8, dataMask)
    /********** handle swap req from pipe end **********/

    /********** handle axi4 master interface end **********/
    val (out, edgeOut) = masternode.out(0)

    val offset = RegInit(0.U(log2Ceil(windowSizePerNp/dataBytes)))
    val idle :: wind_ar_send :: wait_wind_r :: Nil = Enum(3)
    val load_state_R = RegInit(idle)
    val loadMata = RegInit(0.U.asTypeOf(new Bundle {
              val tid = UInt(log2Up(numThread).W)
              val addr = UInt(addrWidth.W)
        }))

    out.ar.valid := false.B
    out.ar.bits.id := 0.U
    out.r.ready := true.B

    switch(load_state_R)
    {
      is(idle) {
        loadpktReqQ.io.deq.ready := true.B
        when(loadpktReqQ.io.deq.fire()) {
          offset := 0.U
          loadMata.tid := loadpktReqQ.io.deq.bits.tid
          loadMata.addr := loadpktReqQ.io.deq.bits.addr
          load_state_R := wind_ar_send
        }
      }
      is(wind_ar_send) {
        out.ar.valid := true.B
        out.ar.bits.addr := loadMata.addr
        when(out.ar.fire())
        { load_state_R := wait_wind_r }
      }
      is(wait_wind_r) {
        when(out.r.fire()) {
          val wdata = VecInit.tabulate(dataBytes) { i => out.r.bits.data(8*(i+1)-1, 8*i) }
          Seq.tabulate(dataBytes) { i=> banks(i).write(offset, wdata(i)) }
          offset := offset + 1.U
          when(out.r.bits.last)
          { load_state_R := idle }
        }
      }
    }

    /********** handle axi4 master interface end **********/
  }
}





