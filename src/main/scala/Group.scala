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


class Group(lid: Int)(implicit p: Parameters) extends LazyModule with NpusParams
{
  val ixbar = LazyModule(new AXI4Xbar)
  val pxbar = LazyModule(new AXI4Xbar)
  val pmasternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"Group_$lid",
                                                      id   = IdRange(0, 1 << 1))))))
  pxbar.node := pmasternode

  val npus = Seq.tabulate(numNpu)
  { i => 
    val npu = LazyModule(new Npu(i)) 
    ixbar.node := npu.imasternode
    pxbar.node := npu.pmasternode
  }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val reset = Input(Bool())
    })
    val (pout, pedge) = pmasternode.out(0)

  }
}







