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
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{GenericLogicalTreeNode,BusMemoryLogicalTreeNode, LogicalModuleTree, LogicalTreeNode}
import freechips.rocketchip.diplomaticobjectmodel.model.AXI4_Lite
import java.nio.file.{Files, Paths}
import java.nio.ByteBuffer

class AXI4Uart(id: Int)(implicit p: Parameters) extends LazyModule with NpusParams
{
  private val address = AddressSet(uartBase, uartSize-1)
  private val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(address),
      //resources     = resources,
      regionType    = if (true) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = true,
      supportsRead  = TransferSizes(1, fetchBytes),
      supportsWrite = TransferSizes(1, fetchBytes),
      interleavedId = Some(0))),
    beatBytes  = mmioBeatBytes,
    requestKeys = if (true) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  val frag = LazyModule(new AXI4Fragmenter)
  node := frag.node

  lazy val module = new LazyModuleImp(this) 
  {
    val (in, edgeIn) = node.in(0)
    chisel3.dontTouch(in)
    val aw_fire_s1 = RegInit(false.B)
    val ar_fire_s1 = RegInit(false.B)
    val aw_id_s1 = RegInit(0.U)
    val ar_id_s1 = RegInit(0.U)
    val uart_reg = RegInit(0x72345678.U(32.W)); chisel3.dontTouch(uart_reg)
    in.aw.ready := true.B
    in.w.ready := true.B
    in.ar.ready := !ar_fire_s1
    

    // handle aw/w->b begin
    when(in.aw.fire() && address.contains(in.aw.bits.addr) && in.w.fire())
    { 
      uart_reg := in.w.bits.data(31,0) 
      aw_fire_s1 := in.aw.fire()
      aw_id_s1 := in.aw.bits.id
    }
    in.b.valid := aw_fire_s1
    in.b.bits.id := aw_id_s1
    when(aw_fire_s1 && in.b.fire())
    { aw_fire_s1 := false.B }
    // handle aw/w->b end

    // handle ar->r begin
    val sel_s0 = address.contains(in.ar.bits.addr); chisel3.dontTouch(sel_s0)
    when(in.ar.fire() && address.contains(in.ar.bits.addr))
    { 
      ar_fire_s1 := true.B
      ar_id_s1 := in.ar.bits.id
    }
    in.r.valid := ar_fire_s1
    in.r.bits.id := ar_id_s1
    in.r.bits.last := true.B
    in.r.bits.data := Cat(0.U(33.W), uart_reg(30,0))
    when(ar_fire_s1 && in.r.fire())
    { ar_fire_s1 := false.B }
    // handle ar->r end

  }
}


class AXI4MatchEngin(id: Int, val address:AddressSet, delay:Int = 10)(implicit p: Parameters) extends LazyModule with NpusParams
{
  private val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(address),
      //resources     = resources,
      regionType    = if (true) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = true,
      supportsRead  = TransferSizes(1, fetchBytes),
      supportsWrite = TransferSizes(1, fetchBytes),
      interleavedId = Some(0))),
    beatBytes  = accBeatBytes,
    requestKeys = if (true) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  val frag = LazyModule(new AXI4Fragmenter)
  node := frag.node
  
  lazy val module = new LazyModuleImp(this) 
  {
    val (in, edgeIn) = node.in(0)
    chisel3.dontTouch(in)
    val w_fire_s1 = RegInit(false.B)
    val aw_id_s1 = RegInit(0.U)
    val tcam_reg = RegInit(0x67654321.U(dataWidth.W)); chisel3.dontTouch(tcam_reg)
    in.aw.ready := !w_fire_s1
    in.w.ready := !w_fire_s1
    in.ar.ready := true.B   

    val IncCounter = Counter(delay)

    in.r.valid := w_fire_s1 && IncCounter.inc()
    in.r.bits.id := aw_id_s1
    in.r.bits.data := tcam_reg
    when(w_fire_s1 && in.r.fire())
    { w_fire_s1 := false.B }

    when(in.aw.fire() && address.contains(in.aw.bits.addr) && in.w.fire())
    { 
      tcam_reg := in.w.bits.data(dataWidth-1,0) 
      w_fire_s1 := in.w.fire()
      aw_id_s1 := in.aw.bits.id
      IncCounter.reset()
    }

  }
}

class FileData(contentsDelayed: => Seq[Byte] ) { def Bytes = contentsDelayed }

// Setting wcorrupt=true is not enough to enable the w.user field
// You must also list AMBACorrupt in your master's requestFields
class AXI4IROM(
    file: String,
    address: AddressSet,
    cacheable: Boolean = true,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) extends LazyModule
{
  private val device = devName
    .map(new SimpleDevice(_, Seq("test.IROM")))
    .getOrElse(new MemoryDevice())

  private val resources = device.reg("mem")

  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = List(address) ++ errors,
      resources     = resources,
      regionType    = if (cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = executable,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    requestKeys = if (wcorrupt) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  val frag = LazyModule(new AXI4Fragmenter)
  node := frag.node

  lazy val module = new LazyModuleImp(this) 
  {
    val (in, edgeIn) = node.in(0)

    private lazy val img_contents = {
              val romdata = Files.readAllBytes(Paths.get(file))
              val rom = ByteBuffer.wrap(romdata)
              rom.array()}

    val contents = new FileData(img_contents).Bytes
    val wrapSize = 1 << log2Ceil(contents.size)
    //require (wrapSize <= depth*datalen)
    val dwords = (contents ++ Seq.fill(wrapSize-contents.size)(0.toByte)).grouped(beatBytes).toSeq
    val bigs = dwords.map(_.foldRight(BigInt(0)){ case (x,y) => (x.toInt & 0xff) | y << 8})
    val rom = Chisel.Vec(bigs.map(x => x.U((8*beatBytes).W)))

    val index = in.ar.bits.addr(log2Ceil(wrapSize)-1,log2Ceil(beatBytes))
    val offset = in.ar.bits.addr(log2Ceil(beatBytes)-1, 0)
    val ar_sel_s0 = address.contains(in.ar.bits.addr);chisel3.dontTouch(ar_sel_s0)
    val ar_sel_s1 = RegNext(ar_sel_s0);chisel3.dontTouch(ar_sel_s1)
    val ar_fire_s1_s1 = RegNext(in.ar.fire() && ar_sel_s0);chisel3.dontTouch(ar_fire_s1_s1)
    val ar_id_s1_s1 = RegNext(in.ar.bits.id);chisel3.dontTouch(ar_id_s1_s1)
    val ar_echo_s1 = RegNext(in.ar.bits.echo);chisel3.dontTouch(ar_echo_s1)
    val r_data = RegNext(rom(index));chisel3.dontTouch(r_data)
    //val r_mask = FillInterleaved(8, Fill(beatBytes, "b1".U) << offset)(beatBytes*8-1,0);chisel3.dontTouch(r_mask)
    
    chisel3.dontTouch(in.ar)
    chisel3.dontTouch(in.r)
    in.ar.ready := true.B
    in.r.valid  := ar_fire_s1_s1
    in.r.bits.id   := ar_id_s1_s1
    in.r.bits.resp := Mux(ar_sel_s1, Mux(/*rcorrupt*/false.B, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY), AXI4Parameters.RESP_DECERR)
    in.r.bits.data := r_data
    in.r.bits.echo := ar_echo_s1
    in.r.bits.last := true.B
  }
}


class AXI4PKTROM(
    file: String,
    address: AddressSet,
    cacheable: Boolean = true,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) extends LazyModule
{
  private val device = devName
    .map(new SimpleDevice(_, Seq("test.PKTROM")))
    .getOrElse(new MemoryDevice())

  private val resources = device.reg("mem")

  private val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = List(address) ++ errors,
      resources     = resources,
      regionType    = if (cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = executable,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    requestKeys = if (wcorrupt) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  val frag = LazyModule(new AXI4Fragmenter)
  node := frag.node

  lazy val module = new LazyModuleImp(this) 
  {
    val (in, edgeIn) = node.in(0)

    private lazy val img_contents = {
              val romdata = Files.readAllBytes(Paths.get(file))
              val rom = ByteBuffer.wrap(romdata)
              rom.array()}

    val contents = new FileData(img_contents).Bytes
    val wrapSize = 1 << log2Ceil(contents.size)
    //require (wrapSize <= depth*datalen)
    val dwords = (contents ++ Seq.fill(wrapSize-contents.size)(0.toByte)).grouped(beatBytes).toSeq
    val bigs = dwords.map(_.foldRight(BigInt(0)){ case (x,y) => (x.toInt & 0xff) | y << 8})
    val rom = Chisel.Vec(bigs.map(x => x.U((8*beatBytes).W)))
    
    /******************* push data to wind begin ******************/
    val idle :: send_data :: Nil = Enum(2)
    val ar_id_s1 = Reg(UInt())
    val ar_echo = Reg(in.ar.bits.echo.cloneType)
    val state = RegInit(idle)
    val offset = RegInit(0.U((log2Ceil(bigs.size +1)).W))
    chisel3.dontTouch(in.ar)
    chisel3.dontTouch(in.r)
    in.ar.ready := true.B
    in.r.valid  := false.B
    in.r.bits.last := false.B

    switch(state)
    {
      is(idle) {
        when(in.ar.fire() && address.contains(in.ar.bits.addr)) {
          //offset := 0.U
          ar_id_s1 := in.ar.bits.id
          ar_echo := in.ar.bits.echo
          state := send_data
        }
      }
      is(send_data) {        
        in.ar.ready := false.B
        in.r.valid := true.B
        in.r.bits.id   := ar_id_s1
        in.r.bits.resp := AXI4Parameters.RESP_OKAY
        in.r.bits.data := rom(offset)(beatBytes*8-1, 0)
        in.r.bits.echo := ar_echo
        in.r.bits.last := false.B
        offset := offset + 1.U
        when(offset(2,0) === 7.U)
        { 
          in.r.bits.last := true.B
          state := idle 
        }
        .otherwise { state := send_data }
      }
    }
    /******************* push data to wind end ******************/
  }
}