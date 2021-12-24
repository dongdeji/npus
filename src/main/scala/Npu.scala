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
  private val front = LazyModule(new FrontEnd(ClusterId, GroupId, NpId))
  private val core = LazyModule(new Core(ClusterId, GroupId, NpId))
  private val accinf = LazyModule(new AccInf(ClusterId, GroupId, NpId))
  val iramxbar = LazyModule(new AXI4Xbar)
  val accxbar = LazyModule(new AXI4Xbar)
  val windxbar = LazyModule(new AXI4Xbar)
  val mmioxbar = LazyModule(new AXI4Xbar)
  iramxbar.node := front.masternode
  iramxbar.node := accinf.accxbar.node
  mmioxbar.node := accinf.accxbar.node
  accxbar.node := accinf.accxbar.node
  core.regfile.slavenode := accinf.regxbar.node
  core.window.slavenode := windxbar.node

  lazy val module = new LazyModuleImp(this) 
  {
    core.module.io.frontend <> front.module.io.core
    accinf.module.io.core <> core.module.io.accinf
  }
}







