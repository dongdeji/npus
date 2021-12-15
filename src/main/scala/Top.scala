/*
 * 
 * A UART is a serial port, also called an RS232 interface.
 * 
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
import java.nio.ByteBuffer
import java.nio.file.{Files,Paths}


trait NpusParams {
  val numCluster: Int = 2
  val numGroup: Int = 2
  val numNpu: Int = 2
  val numThread: Int = 2
  val numIram: Int = 1
  val iramBase: BigInt = 0x2000000
  val iramSize: BigInt = 4096
  val instrBytes: Int = 4
  val fetchInstrs: Int = 8
  val xLenb: Int = 64
  val reset_vector: Int = 0x2000430
  val pcWidth = 32
  
  val fetchBytes = instrBytes*fetchInstrs
  val fetchWidth = fetchBytes*8
  val instrWidth = instrBytes*8
  //val fetchAddrOffWidth = log2Up(fetchBytes)
  //val instrAddrOffWidth = log2Up(instrBytes)
  val tidWidth = log2Up(numThread)
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
  val pxbar = LazyModule(new AXI4Xbar)
  matchslavenode := pxbar.node := AXI4IdIndexer(1/*fifoBits*/) :=  masternode

  val clusters = Seq.tabulate(numCluster) 
  { i => 
    val cluster = LazyModule(new Cluster(i)) 
    pxbar.node := cluster.pxbar.node
    cluster
  }

  lazy val module = new LazyModuleImp(this) {
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






