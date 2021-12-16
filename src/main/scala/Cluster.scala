/*
 * 
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
    (group.ixbar, group.pxbar, group.wxbar)
  }
  /* connect irams and groups */
  for(i <- 0 until iramxbars.size; j <- 0 until groupxbars.size )
  { iramxbars(i) := groupxbars(j)._1.node }

  /* connect match engin*/
  val pxbar = LazyModule(new AXI4Xbar)
  for(j <- 0 until groupxbars.size )
  { pxbar.node := groupxbars(j)._2.node }

  /* connect window */
  val wxbar = LazyModule(new AXI4Xbar)
  for(j <- 0 until groupxbars.size )
  { groupxbars(j)._3.node := wxbar.node }

  val masternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"Cluster-$ClusterId",
                                                      id   = IdRange(0, 1 << 1))))))
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

  slavenode := pxbar.node := AXI4IdIndexer(1/*fifoBits*/) :=  masternode

  lazy val module = new LazyModuleImp(this) {
    chisel3.dontTouch(clock)
    chisel3.dontTouch(reset)
    val (out, outedge) = masternode.out(0)
    val (in, inedge) = slavenode.in(0)
  }
}







