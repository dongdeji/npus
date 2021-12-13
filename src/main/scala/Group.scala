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


class Group(lid: Int)(implicit p: Parameters) extends LazyModule 
{
  val ixbar = AXI4Xbar() 
  val pxbar = AXI4Xbar()
  val pmasternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"Group_$lid",
                                                      id   = IdRange(0, 1 << 1))))))
  pxbar := pmasternode

  val npus = Seq.tabulate(8)
  { i => 
    val npu = LazyModule(new Npu(i)) 
    ixbar := npu.imasternode
    pxbar := npu.pmasternode
  }

  lazy val module = new LazyModuleImp(this) {
    val (pout, pedge) = pmasternode.out(0)

  }
}







