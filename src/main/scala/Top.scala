/*
 to do
 */

package npus

import chisel3._
import chisel3.util._

import freechips.rocketchip.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.diplomacy.{ AddressSet, LazyModule, LazyModuleImp, RegionType, LazyRawModuleImp, BigIntHexContext}
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

  val iramGlobalBase: BigInt = x"0001_0000"
  val iramSizePerCluster: BigInt = x"1000"
  require(true == isPow2(iramSizePerCluster)) 
  require(0 == (iramGlobalBase % iramSizePerCluster))

  val dramGlobalBase: BigInt = x"0002_0000"
  val dramSizePerNp: BigInt = x"1000"
  require(true == isPow2(dramSizePerNp)) 
  require(0 == (dramGlobalBase % dramSizePerNp))

  val windowGlobalBase: BigInt = x"0020_0000"
  val windowSizePerNp: BigInt = x"200"*numThread
  require(true == isPow2(windowSizePerNp))
  require(0 == (windowGlobalBase % windowSizePerNp))

  val isaRegNumPerThread: Int = 32
  val regfileGlobalBase: BigInt = x"0040_0000"
  val regfileSizePerNp: BigInt = isaRegNumPerThread*dataBytes*numThread
  require(true == isPow2(regfileSizePerNp))
  require(0 == (regfileGlobalBase % regfileSizePerNp))

  val uartBase: BigInt = x"5400_0000"
  val uartSize: BigInt = x"1000"
  require(true == isPow2(uartSize))
  require(0 == (uartBase % uartSize))

  val tcamBase: BigInt = x"5401_0000"
  val tcamSize: BigInt = x"100"
  require(true == isPow2(tcamSize))
  require(0 == (tcamBase % tcamSize))

  val test1Base: BigInt = x"7000_0000"
  val test1Size: BigInt = x"400"
  require(true == isPow2(test1Size))
  require(0 == (test1Base % test1Size))

  val test2Base: BigInt = x"7100_0000"
  val test2Size: BigInt = x"400"
  require(true == isPow2(test2Size))
  require(0 == (test2Base % test2Size))

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
  private val matchslavenode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(AddressSet(test1Base, test1Size - 1)),
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
  matchslavenode := accxbar.node := masternode
  val windxbar = LazyModule(new AXI4Xbar)
  windxbar.node := AXI4IdIndexer(1/*fifoBits*/) :=  wmasternode

  val mmioxbar = LazyModule(new AXI4Xbar)
  val uart = LazyModule(new Axi4Uart(0))
  uart.slavenode := mmioxbar.node

  val clusters = Seq.tabulate(numCluster) 
  { i => 
    val cluster = LazyModule(new Cluster(i)) 
    accxbar.node := cluster.accxbar.node
    mmioxbar.node := cluster.mmioxbar.node
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






