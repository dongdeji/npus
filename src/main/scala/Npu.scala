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


class Npu(ClusterId:Int, GroupId:Int, NpId: Int)(implicit p: Parameters) extends LazyModule with NpusParams 
{
  val front = LazyModule(new FrontEnd(ClusterId, GroupId, NpId))
  val core = LazyModule(new Core(ClusterId, GroupId, NpId))
  val accinf = LazyModule(new AccInf(ClusterId, GroupId, NpId))
  val ixbar = LazyModule(new AXI4Xbar)
  val pxbar = LazyModule(new AXI4Xbar)
  val wxbar = LazyModule(new AXI4Xbar)
  ixbar.node := front.masternode
  pxbar.node := accinf.pmasternode
  core.window.slavenode := wxbar.node

  core.regfile.slavenode := accinf.rxbar.node

  lazy val module = new LazyModuleImp(this) {
    //val (iout, iedge) = imasternode.out(0)
    //val (pout, pedge) = pmasternode.out(0)
    core.module.io.frontend <> front.module.io.core
    accinf.module.io.core <> core.module.io.accinf
  }
}







