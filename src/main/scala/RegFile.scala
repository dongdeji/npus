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


class RegFiles extends Module with NpusParams 
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

  val part1 = Seq.tabulate(dataBytes) { i => SyncReadMem(numThread*32, UInt(8.W)) }
  val part2 = Seq.tabulate(dataBytes) { i => SyncReadMem(numThread*32, UInt(8.W)) }
  
  when(io.rd_write)
  {
    Seq.tabulate(dataBytes){ i => part1(i).write(io.rd, io.rd_data(i*8+7, i*8)) }
    Seq.tabulate(dataBytes){ i => part2(i).write(io.rd, io.rd_data(i*8+7, i*8)) }
  }
  // x0 is always 0
  io.rs1_data := Mux(io.rs1(4, 0) === 0.U, 0.U, VecInit(Seq.tabulate(dataBytes){ i => part1(i).read(io.rs1)}).asUInt)
  io.rs2_data := Mux(io.rs2(4, 0) === 0.U, 0.U, VecInit(Seq.tabulate(dataBytes){ i => part2(i).read(io.rs2)}).asUInt)
}







