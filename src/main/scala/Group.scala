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


class Group(ClusterId:Int, GroupId:Int)(implicit p: Parameters) extends LazyModule with NpusParams
{
  val ixbar = LazyModule(new AXI4Xbar)
  val pxbar = LazyModule(new AXI4Xbar)
  val wxbar = LazyModule(new AXI4Xbar)
  val pmasternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"Group-$ClusterId-$GroupId",
                                                      id   = IdRange(0, 1 << 1))))))
  pxbar.node := pmasternode

  val npus = Seq.tabulate(numNpu)
  { i => 
    val npu = LazyModule(new Npu(ClusterId, GroupId, i)) 
    ixbar.node := npu.imasternode
    pxbar.node := npu.pmasternode
    npu.windownode := wxbar.node
  }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val reset = Input(Bool())
    })
    val (pout, pedge) = pmasternode.out(0)

  }
}







