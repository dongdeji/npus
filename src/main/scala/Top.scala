/*
 to do
 */

package npus

import chisel3._
import chisel3.util._

import freechips.rocketchip.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.diplomacy.{ AddressSet, LazyModule, LazyModuleImp, RegionType, LazyRawModuleImp }
import freechips.rocketchip.tile._
import chisel3.experimental.chiselName


trait NpusParams {
  val numCluster: Int = 1
  val numGroup: Int = 1
  val numNpu: Int = 1
  val numThread: Int = 1
  val numIramBank: Int = 1

  val instrBytes: Int = 4
  val fetchInstrs: Int = 4
  
  val dataWidth: Int = 64
  val dataBytes = dataWidth/8
  val addrWidth = 32

  val fetchBytes = instrBytes*fetchInstrs
  val fetchWidth = fetchBytes*8
  val instrWidth = instrBytes*8
  val tidWidth = log2Up(numThread)

  val iramGlobalBase: BigInt = 0x10000
  val iramSizePerCluster: BigInt = 0x1000  
  require(true == isPow2(iramGlobalBase)) 
  require(true == isPow2(iramSizePerCluster))

  val dramGlobalBase: BigInt = 0x20000
  val dramSizePerNp: BigInt = 0x1000
  require(true == isPow2(dramGlobalBase)) 
  require(true == isPow2(dramSizePerNp))

  val windowGlobalBase: BigInt = 0x200000
  val windowSizePerNp: BigInt = 0x200*numThread
  require(true == isPow2(windowGlobalBase)) 
  require(true == isPow2(windowSizePerNp))

  val regfileGlobalBase: BigInt = 0x400000
  val regfileSizePerNp: BigInt = 32*dataBytes*numThread
  require(true == isPow2(regfileGlobalBase)) 
  require(true == isPow2(regfileSizePerNp))


  val memInstrHalt = true

  def bigBits(x: BigInt, tail: List[Boolean] = Nil): List[Boolean] =
    if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)
  def mask(address: AddressSet, dataBytes:Int): List[Boolean] = bigBits(address.mask >> log2Ceil(dataBytes))
  def haltCondition(instr:UInt): Bool =
  {
    val memInstrhalt_list = if(memInstrHalt) Seq(LB.value.asUInt()(6,0), SB.value.asUInt()(6,0)) else Nil
    val halt_list = Seq(BEQ.value.asUInt()(6,0), JAL.value.asUInt()(6,0), JALR.value.asUInt()(6,0)) ++ memInstrhalt_list
    instr(6,0).isOneOf(halt_list)
  }
}


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




class npusTop()(implicit p: Parameters) extends LazyModule with NpusParams 
{

  ElaborationArtefacts.add("graphml", graphML)
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
  
  val masternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                        masters = Seq(AXI4MasterParameters(
                                                        name = s"Cluster_test",
                                                        id   = IdRange(0, 1 << 1))))))
  val wmasternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                        masters = Seq(AXI4MasterParameters(
                                                        name = s"Window_test",
                                                        id   = IdRange(0, 1 << 1))))))
  val matchslavenode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(AddressSet(0x7000000 + 0x400, 0x3ff)),
      //resources     = resources,
      regionType    = if (true) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = true,
      supportsRead  = TransferSizes(1, fetchBytes),
      supportsWrite = TransferSizes(1, fetchBytes),
      interleavedId = Some(0))),
    beatBytes  = fetchBytes,
    requestKeys = if (true) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))
  val accxbar = LazyModule(new AXI4Xbar)
  matchslavenode := accxbar.node := AXI4IdIndexer(1/*fifoBits*/) :=  masternode
  val windxbar = LazyModule(new AXI4Xbar)
  windxbar.node := AXI4IdIndexer(1/*fifoBits*/) :=  wmasternode

  val clusters = Seq.tabulate(numCluster) 
  { i => 
    val cluster = LazyModule(new Cluster(i)) 
    accxbar.node := cluster.accxbar.node
    cluster.windxbar.node := windxbar.node
    cluster
  }

  lazy val module = new LazyModuleImp(this) 
  {
    val io = IO(new Bundle {
      val success = Output(Bool())
      val start = Input(Bool())
    })

    val matchperph = matchslavenode.makeIOs()

  }
}

object TopMain extends App with HasRocketChipStageUtils {
  override def main(args: Array[String]): Unit = {
    implicit val p = Parameters.empty
    (new chisel3.stage.ChiselStage).execute(args, Seq(
      chisel3.stage.ChiselGeneratorAnnotation(() => {
        val soc = LazyModule(new npusTop())
        soc.module
      })
    ))

    ElaborationArtefacts.files.foreach{ case (extension, contents) =>
      writeOutputFile("./generated-src", s"npusTop.${extension}", contents())
    }
  }
}






