/*
 * 
 * A UART is a serial port, also called an RS232 interface.
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
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.tile._
import chisel3.experimental.chiselName
import java.nio.ByteBuffer
import java.nio.file.{Files,Paths}


class StoreGen(typ: UInt, addr: UInt, dat: UInt, maxSize: Int = 8) 
{
  val size = typ(log2Up(log2Up(maxSize)+1)-1,0)

  def misaligned =
          (addr & ((1.U << size) - 1.U)(log2Up(maxSize)-1,0)).orR

  def mask = {
      var res = 1.U

      for (i <- 0 until log2Up(maxSize)) {
        val upper = Mux(addr(i), res, 0.U) | Mux(size >= (i+1).U, ((BigInt(1) << (1 << i))-1).U, 0.U)
        val lower = Mux(addr(i), 0.U, res)
        res = Cat(upper, lower)
      }

      res
  }
  protected def genData(i: Int): UInt =
            if (i >= log2Up(maxSize)) dat
            else Mux(size === i.U, Fill(1 << (log2Up(maxSize)-i), dat((8 << i)-1,0)), genData(i+1))

  def data = genData(0)
  def wordData = genData(2)
}


class AcceInfBundle extends Bundle with NpusParams 
{
  val req = Valid( new Bundle {
                    val cmd = UInt(M_SZ.W) /* dmem_req.ctrl.mem_cmd */
                    val size = UInt(2.W) /* dmem_req.inst_32(13,12) */
                    val signed = UInt(1.W) /* !dmem_req.inst_32(14) */
                    val data = UInt(dataWidth.W)
                    val addr = UInt(32.W)
                    val tid = UInt(log2Up(numThread).W) })

  val resp = Flipped( Valid( new Bundle {
                    val data = UInt(dataWidth.W)
                    val addr = UInt(32.W)
                    val tid = UInt(log2Up(numThread).W) }))

  override def cloneType: this.type = (new AcceInfBundle).asInstanceOf[this.type]
}

class AcceInf(ClusterId:Int, GroupId:Int, NpId: Int)(implicit p: Parameters) extends LazyModule with NpusParams 
{
  val masternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"Core",
                                                      id = IdRange(0, 1 << 1))))))
  val window = LazyModule(new Window(ClusterId, GroupId, NpId))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val core = Flipped(new AcceInfBundle) 
    })
    chisel3.dontTouch(io)
    val (out, edge) = masternode.out(0)


  }
}







