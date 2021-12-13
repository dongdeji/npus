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
  val numThread: Int = 8
  val inslenb: Int = 32  
  val fetchWidthB: Int = 16
  val xLenb: Int = 64
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
      supportsRead  = TransferSizes(1, fetchWidthB),
      supportsWrite = TransferSizes(1, fetchWidthB),
      interleavedId = Some(0))),
    beatBytes  = fetchWidthB,
    requestKeys = if (true) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))
  val pxbar = AXI4Xbar()
  matchslavenode := pxbar := AXI4IdIndexer(1/*fifoBits*/) :=  masternode

  val clusters = Seq.tabulate(2) 
  { i => 
    val cluster = LazyModule(new Cluster(i)) 
    pxbar := cluster.pxbar
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






