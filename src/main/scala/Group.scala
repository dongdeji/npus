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
  val iramxbar = LazyModule(new AXI4Xbar)
  val accxbar = LazyModule(new AXI4Xbar)
  val windxbar = LazyModule(new AXI4Xbar)

  val npus = Seq.tabulate(numNpu)
  { i => 
    val npu = LazyModule(new Npu(ClusterId, GroupId, i)) 
    iramxbar.node := npu.iramxbar.node
    accxbar.node := npu.accxbar.node
    npu.windxbar.node := windxbar.node
  }

  lazy val module = new LazyModuleImp(this) 
  {
    // to do by dongdeji
  }
}







