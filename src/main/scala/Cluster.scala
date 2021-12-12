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


class Cluster(id: Int)(implicit p: Parameters) extends LazyModule 
{
  val axi4xbar = AXI4Xbar()
  val beatBytes = 8

  /* iram sequence */
  val iramxbars = Seq.tabulate(8) 
  { i => 
    val iramxbar = AXI4Xbar()
    val iram = LazyModule(new AXI4ROM(AddressSet(0x10000 + 0x400*i, 0x3ff), beatBytes = beatBytes))
    iram.node := iramxbar
    iramxbar
  }
  iramxbars.foreach { x => (x := AXI4Buffer(BufferParams.flow) := axi4xbar) }
  val masternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"Cluster_$id",
                                                      id   = IdRange(0, 1 << 1))))))
  val slavenode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(AddressSet(0x20000 + 0x400, 0x3ff)),
      //resources     = resources,
      regionType    = if (true) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = true,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    requestKeys = if (true) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))
  slavenode := axi4xbar := AXI4IdIndexer(1/*fifoBits*/) :=  masternode

  /* iram sequence */
  val groupxbars = Seq.tabulate(2) 
  { i => LazyModule(new Group(i)).xbar }

  for(i <- 0 until iramxbars.size; j <- 0 until groupxbars.size )
  { iramxbars(i) := groupxbars(j) }

  lazy val module = new LazyModuleImp(this) {
    val (out, edge) = masternode.out(0)
  }
}







