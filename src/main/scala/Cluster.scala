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
  private val iramxbars = Seq.tabulate(numIramBank) 
  { i => 
    val iramPerBankSize = iramSizePerCluster/numIramBank
    require(0 == iramSizePerCluster%numIramBank )
    require(true == isPow2(iramPerBankSize))
    val iramxbar = LazyModule(new AXI4Xbar)
    val iram = LazyModule(new AXI4IROM(file = "./bootrom/bootrom.img",
                                       address = AddressSet(iramGlobalBase + iramSizePerCluster*ClusterId + iramPerBankSize*i, iramPerBankSize-1), 
                                       beatBytes = fetchBytes))
    iram.frag.node := iramxbar.node
    iramxbar
  }
  /* groups */
  private val groupxbars = Seq.tabulate(numGroup) 
  { i => 
    val group = LazyModule(new Group(ClusterId, i))
    (group.iramxbar, group.accxbar, group.mmioxbar)
  }
  /* connect irams and groups */
  for(i <- 0 until iramxbars.size; j <- 0 until groupxbars.size )
  { iramxbars(i).node := groupxbars(j)._1.node }

  /* connect fbus */
  val fbusxbar = LazyModule(new AXI4Xbar)
  for(j <- 0 until iramxbars.size )
  { iramxbars(j).node := fbusxbar.node }

  /* connect match engin*/
  val accxbar = LazyModule(new AXI4Xbar)
  for(j <- 0 until groupxbars.size )
  { accxbar.node := groupxbars(j)._2.node }

  /* connect mmio */
  val mmioxbar = LazyModule(new AXI4Xbar)
  for(j <- 0 until groupxbars.size )
  { mmioxbar.node := groupxbars(j)._3.node }

  lazy val module = new LazyModuleImp(this) 
  {
    // to do by dongdeji
  }
}







