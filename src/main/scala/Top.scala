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
import NpInstructions._

trait NpusParams {
  val supportNpInstr: Boolean = true
  val numCluster: Int = 1
  val numGroup: Int = 1
  val numNpu: Int = 1
  val numThread: Int = 1
  val numIramBank: Int = 1

  val instBytes: Int = 4
  val fetchInstrs: Int = 4
  
  val dataWidth: Int = 64
  val dataBytes = dataWidth/8
  val addrWidth = 32

  val fetchBytes = instBytes*fetchInstrs
  val fetchWidth = fetchBytes*8
  val instWidth = instBytes*8
  val tidWidth = log2Up(numThread)

  def RequireAddressAlign(base: BigInt, size: BigInt) = 
  { 
    require(true == isPow2(size), s"size:${size} is not Pow2 !") 
    require(0 == (base % size), s"base:${base} is not align to size:${size} !")
  }

  val iramGlobalBase:     BigInt = x"0001_0000"
  val iramSizePerCluster: BigInt = x"0000_1000"
  RequireAddressAlign(iramGlobalBase, iramSizePerCluster)

  val dramGlobalBase: BigInt = x"0002_0000"
  val dramSizePerNp:  BigInt = x"0000_1000"
  RequireAddressAlign(dramGlobalBase, dramSizePerNp)

  val keyBuffBase:       BigInt = x"0002_0000"
  val keyBuffSizePerNp:  BigInt = x"0000_1000"
  RequireAddressAlign(keyBuffBase, keyBuffSizePerNp)

  val windowGlobalBase: BigInt = x"0020_0000"
  val windowSizePerNp:  BigInt = x"0000_0200"*numThread
  RequireAddressAlign(windowGlobalBase, windowSizePerNp)

  val pktBuffBase: BigInt = x"003f_fff0"
  val pktBuffSize:  BigInt = x"0000_0010"*numThread
  RequireAddressAlign(pktBuffBase, pktBuffSize)

  val isaRegNumPerThread: Int = 32
  val regfileGlobalBase: BigInt = x"0040_0000"
  val regfileSizePerNp:  BigInt = isaRegNumPerThread*dataBytes*numThread
  RequireAddressAlign(regfileGlobalBase, regfileSizePerNp)

  val uartBase: BigInt = x"5400_0000"
  val uartSize: BigInt = x"0000_1000"
  RequireAddressAlign(uartBase, uartSize)

  val tcamBase: BigInt = x"5401_0000"
  val tcamSize: BigInt = x"0000_0100"
  RequireAddressAlign(tcamBase, tcamSize)

  val lramBase: BigInt = x"5402_0000"
  val lramSize: BigInt = x"0000_0100"
  RequireAddressAlign(lramBase, lramSize)

  val eamBase: BigInt = x"5403_0000"
  val eamSize: BigInt = x"0000_0100"
  RequireAddressAlign(eamBase, eamSize)

  val test1Base: BigInt = x"7000_0000"
  val test1Size: BigInt = x"0000_0400"
  RequireAddressAlign(test1Base, test1Size)

  val test2Base: BigInt = x"7100_0000"
  val test2Size: BigInt = x"0000_0400"
  RequireAddressAlign(test2Base, test2Size)

  val memInstrHalt = true

  def bigBits(x: BigInt, tail: List[Boolean] = Nil): List[Boolean] =
    if (x == 0) tail.reverse else bigBits(x >> 1, ((x & 1) == 1) :: tail)
  def mask(address: AddressSet, dataBytes:Int): List[Boolean] = bigBits(address.mask >> log2Ceil(dataBytes))
  def haltCondition(inst:UInt): Bool =
  {
    val memInstrHalt_list = if(memInstrHalt) Seq(LB.value.asUInt()(6,0), SB.value.asUInt()(6,0)) else Nil
    val halt_list = Seq(BEQ.value.asUInt()(6,0), JAL.value.asUInt()(6,0), JALR.value.asUInt()(6,0)) ++ memInstrHalt_list
    val accInstrHaltCondition = 
        if(supportNpInstr) 
        { ((inst(6,0) === LPKTWJAL.value.asUInt()(6,0)) && (inst(31,29) === LPKTWJAL.value.asUInt()(31,29))) }
        else { false.B }

    inst(6,0).isOneOf(halt_list) || accInstrHaltCondition
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
  
  private val fbusmaster = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                        masters = Seq(AXI4MasterParameters(
                                                        name = s"FrontBuss_src",
                                                        id   = IdRange(0, 1 << 1))))))
  private val accxbar = LazyModule(new AXI4Xbar)
  private val tcam = LazyModule(new Axi4Tcam(0))
  private val lram = LazyModule(new Axi4Lram(0))
  tcam.frag.node := accxbar.node
  lram.frag.node := accxbar.node

  private val mmioxbar = LazyModule(new AXI4Xbar)
  private val pktbuff = LazyModule(new AXI4PKTROM(file = "./src/test/resources/vxlan_pkts.img",
                                     address = AddressSet(pktBuffBase, pktBuffSize-1), 
                                     beatBytes = fetchBytes))
  pktbuff.frag.node := mmioxbar.node
  private val uart = LazyModule(new Axi4Uart(0))
  uart.frag.node := mmioxbar.node
  
  private val clusters = Seq.tabulate(numCluster) 
  { i => 
    val cluster = LazyModule(new Cluster(i)) 
    accxbar.node := cluster.accxbar.node
    mmioxbar.node := cluster.mmioxbar.node
    cluster.fbusxbar.node := fbusmaster
    cluster
  }

  lazy val module = new LazyModuleImp(this) 
  {
    val io = IO(new Bundle {
      val success = Output(Bool())
      val start = Input(Bool())
    })
    val debugCounter = Counter(30000)
    io.success := debugCounter.inc
    // to do by dongdeji

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






