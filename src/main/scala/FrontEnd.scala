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


class FrontEnd(implicit p: Parameters) extends LazyModule 
{
  val masternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"FrontEnd",
                                                      id   = IdRange(0, 1 << 1))))))
  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val test = Output(UInt(8.W))
    })
    chisel3.dontTouch(io)
    val (out, edge) = masternode.out(0)


  }
}







