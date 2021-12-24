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
//import freechips.rocketchip.rocket.Instructions._
//import freechips.rocketchip.tile._
import chisel3.experimental.chiselName

class CSRFile extends Module with NpusParams
{
  val io = IO(new Bundle() {
                  val tid = Input(UInt(log2Up(numThread).W))
                  val rw = new Bundle {
                  val addr = Input(UInt(12.W/*CSR.ADDRSZ*/))
                  val cmd = Input(UInt(3.W/*CSR.SZ*/))
                  val wdata = Input(UInt(dataWidth.W))
                  val rdata = Output(UInt(dataWidth.W))
                  }
            })

  io.rw.rdata := (-1.S(dataWidth.W)).asUInt

  when(io.rw.cmd.isOneOf(CSR.S, CSR.C, CSR.W) && io.rw.addr === CSRs.mhartid.U(12.W))
  { io.rw.rdata := io.tid }
}





