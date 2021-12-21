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


class Cluster(ClusterId:Int)(implicit p: Parameters) extends LazyModule with NpusParams 
{
  /* iram sequence */
  val iramxbars = Seq.tabulate(numIram) 
  { i => 
    val iramxbar = LazyModule(new AXI4Xbar)
    val iram = LazyModule(new AXI4ROM(AddressSet(iramBase + iramSize*i, iramSize-1), beatBytes = fetchBytes))
    iram.node := iramxbar.node
    iramxbar.node
  }
  /* groups */
  val groupxbars = Seq.tabulate(numGroup) 
  { i => 
    val group = LazyModule(new Group(ClusterId, i))
    (group.iramxbar, group.accxbar, group.windxbar)
  }
  /* connect irams and groups */
  for(i <- 0 until iramxbars.size; j <- 0 until groupxbars.size )
  { iramxbars(i) := groupxbars(j)._1.node }

  /* connect match engin*/
  val accxbar = LazyModule(new AXI4Xbar)
  for(j <- 0 until groupxbars.size )
  { accxbar.node := groupxbars(j)._2.node }

  /* connect window */
  val windxbar = LazyModule(new AXI4Xbar)
  for(j <- 0 until groupxbars.size )
  { groupxbars(j)._3.node := windxbar.node }

  val slavenode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(AddressSet(0x8000000 + 0x400, 0x3ff)),
      //resources     = resources,
      regionType    = if (true) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = true,
      supportsRead  = TransferSizes(1, fetchBytes),
      supportsWrite = TransferSizes(1, fetchBytes),
      interleavedId = Some(0))),
    beatBytes  = fetchBytes,
    requestKeys = if (true) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  slavenode := accxbar.node

  lazy val module = new LazyModuleImp(this) 
  {
    // to do by dongdeji
    val (in, edgeIn) = slavenode.in(0)
  }
}







