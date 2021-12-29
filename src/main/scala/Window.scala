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
    val offsetWidth = 1 << log2Ceil(log2Ceil(windowSizePerNp))
    val io = IO(new Bundle {
      val swap = Flipped(new SwapWindBundle(offsetWidth))      
      val accLoad = Flipped(new AccLoadBundle)
    })
    chisel3.dontTouch(io)

    val accLoadReqQ = Module(new Queue(io.accLoad.req.bits.cloneType, numThread + 1, flow = true))
    accLoadReqQ.io.enq.valid := io.accLoad.req.valid
    accLoadReqQ.io.enq.bits := io.accLoad.req.bits
    accLoadReqQ.io.deq.ready := false.B
    // to do by dongdeji

    val banks = Seq.tabulate(dataBytes) { i => SyncReadMem(windowSizePerNp/dataBytes, UInt(8.W)) }
    
    /********** handle swap req from pipe begin **********/
    val banks_offset = VecInit(Seq.tabulate(dataBytes){ i => (io.swap.offset + i.U)(offsetWidth-1, 0)}).asUInt
    val group_offset = io.swap.offset(log2Ceil(dataBytes)-1, 0)
    val banks_offset_wide = banks_offset << (group_offset << log2Ceil(offsetWidth))
    val banks_offset_pakage = (banks_offset_wide >> offsetWidth*dataBytes) | banks_offset_wide(offsetWidth*dataBytes-1,0)
    val group_offset_s1 = RegNext(group_offset)
    val size_s1 = RegNext(io.swap.size)
    val signed_s1 = RegNext(io.swap.signed)
    val data_pakage = VecInit(Seq.tabulate(dataBytes) { i => 
                  val offset = banks_offset_pakage((i+1)*offsetWidth-1, i*offsetWidth)
                  banks(i).read(offset >> log2Ceil(dataBytes)) }).asUInt
    val data_raw_l = data_pakage >> (group_offset_s1 << log2Ceil(8))
    val data_raw_h = (data_pakage << ((dataBytes.U - group_offset_s1) << log2Ceil(8)))(dataWidth - 1, 0)
    val allBitOne = ~(0.U(dataBytes.W))
    val dataMask = ~( (allBitOne << (1.U << size_s1)) (dataBytes-1,0) )
    io.swap.data := (data_raw_h | data_raw_l) & FillInterleaved(8, dataMask)
    /********** handle swap req from pipe end **********/

    /********** handle axi4 master interface end **********/
    val (out, edgeOut) = masternode.out(0)

    val offset = RegInit(0.U(log2Ceil(windowSizePerNp/dataBytes).W))
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
        accLoadReqQ.io.deq.ready := true.B
        when(accLoadReqQ.io.deq.fire()) {
          offset := 0.U
          loadMata.tid := accLoadReqQ.io.deq.bits.tid
          loadMata.addr := accLoadReqQ.io.deq.bits.addr
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





