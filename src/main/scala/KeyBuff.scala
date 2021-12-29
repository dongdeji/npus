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
    val offsetWidth = 1 << log2Ceil(log2Ceil(keyBuffSizePerNp))
    val io = IO(new Bundle {
      val core = Flipped(Valid(new KeyBufReqBundle))
      val sizes = Output(Vec(numThread, UInt(log2Ceil(keyBuffSizePerNp/numThread).W)))
      //val read_addr = Input(UInt(offsetWidth.W))
      //val read_tid = Input(UInt(log2Up(numThread).W))
      //val read_data = Output(UInt(dataWidth.W))
      //val resetBuffs = Input(Vec(numThread, Bool()))
    })
    chisel3.dontTouch(io)

    val Id = ClusterId*numGroup*numNpu + GroupId*numNpu + NpId
    val keyBuffAddress = AddressSet(keyBuffBase + keyBuffSizePerNp*Id, keyBuffSizePerNp-1)
    val heads = RegInit(VecInit(Seq.fill(numThread)(0.U(offsetWidth.W))))
    val head = heads(io.core.bits.tid)    
    //chisel3.dontTouch(head)
    val ramDepth = keyBuffSizePerNp/dataBytes
    val banks = (0 until dataBytes ).map{ i => SyncReadMem(ramDepth, UInt(8.W)) }
    /********** handle swap req from pipe begin **********/
    val banks_offset = VecInit(Seq.tabulate(dataBytes){ i => (head + i.U)(offsetWidth-1, 0)}).asUInt
    val banks_offset_wide = banks_offset << (head(log2Ceil(dataBytes)-1, 0) << log2Ceil(offsetWidth))
    val banks_offset_pakage = (banks_offset_wide >> dataBytes*offsetWidth) | banks_offset_wide(dataBytes*offsetWidth-1,0)
    chisel3.dontTouch(banks_offset_pakage)

    val allBitOne = ~(0.U(dataBytes.W))
    val dataMask = ~( (allBitOne << (1.U << io.core.bits.size)) (dataBytes-1,0) )
    val dataMask_wide = dataMask << head(log2Ceil(dataBytes)-1,0)
    val dataMask_pakage = (dataMask_wide >> dataBytes) | dataMask_wide(dataBytes-1, 0)
    chisel3.dontTouch(dataMask_pakage)

    val data = io.core.bits.data
    val data_wide = dataMask << (head(log2Ceil(dataBytes)-1,0) << log2Ceil(8))
    val data_pakage = data_wide >> (dataBytes*8) | data_wide(dataBytes*8-1, 0)

    Seq.tabulate(dataBytes){ i =>
      val addr = banks_offset_pakage((i+1)*offsetWidth-1, i*offsetWidth)
      val offset = Cat(io.core.bits.tid, addr(offsetWidth-1,log2Ceil(dataBytes)))
      val wdata = data_pakage((i+1)*8-1, i*8)
      when(dataMask_pakage(i) && io.core.fire()) 
      { 
        head := head + 1.U
        banks(i).write(offset, wdata) 
      }
    }
    when(io.core.fire()) { head := head + (1.U << io.core.bits.size) }

    io.sizes := heads
}







