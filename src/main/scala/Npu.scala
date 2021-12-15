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


class Npu(lid: Int)(implicit p: Parameters) extends LazyModule with NpusParams 
{
  val front = LazyModule(new FrontEnd)
  val core = LazyModule(new Core)
  val imasternode = front.masternode
  val pmasternode = core.masternode

  lazy val module = new LazyModuleImp(this) {
    //val (iout, iedge) = imasternode.out(0)
    //val (pout, pedge) = pmasternode.out(0)
    core.module.io <> front.module.io
  }
}







