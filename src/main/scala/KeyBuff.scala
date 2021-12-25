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


class KeyBuff(ClusterId:Int, GroupId:Int, NpId: Int)(implicit p: Parameters) extends Module with NpusParams 
{
    val io = IO(new Bundle {
      val core = Flipped(new AccInfBundle)
    })

    val Id = ClusterId*numGroup*numNpu + GroupId*numNpu + NpId
    val keyBuffAddress = AddressSet(keyBuffBase + keyBuffSizePerNp*Id, keyBuffSizePerNp-1)

    /***************** handle dmem req begin *****************/
    val req_valid_s1 = RegNext(io.core.req.valid)
    val req_cmd_s1 = RegNext(io.core.req.bits.cmd)
    val req_addr_s1 = RegNext(io.core.req.bits.addr)
    val req_tid_s1 = RegNext(io.core.req.bits.tid)
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

    io.core.resp.valid := req_valid_s1 && keyBuffAddress.contains(req_addr_s1)
    io.core.resp.bits.data := rdata >> (req_addr_s1(log2Ceil(dataBytes)-1, 0) << log2Ceil(8))
    io.core.resp.bits.addr := req_addr_s1
    io.core.resp.bits.tid := req_tid_s1 

    /***************** handle dmem req end *****************/

    io.core.readys.valid := io.core.resp.valid
    io.core.readys.bits.thread := io.core.resp.valid << req_tid_s1
}







