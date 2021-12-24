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


class Axi4Uart(id: Int)(implicit p: Parameters) extends LazyModule with NpusParams
{
  val address = AddressSet(uartBase, uartSize-1)
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(address),
      //resources     = resources,
      regionType    = if (true) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = true,
      supportsRead  = TransferSizes(1, fetchBytes),
      supportsWrite = TransferSizes(1, fetchBytes),
      interleavedId = Some(0))),
    beatBytes  = fetchBytes,
    requestKeys = if (true) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  lazy val module = new LazyModuleImp(this) 
  {
    val (in, edgeIn) = node.in(0)
    chisel3.dontTouch(in)
    val uart_reg = RegInit(0x72345678.U(32.W)); chisel3.dontTouch(uart_reg)
    in.aw.ready := true.B
    in.w.ready := true.B
    in.ar.ready := true.B
    
    val aw_fire = RegInit(false.B)
    val ar_fire = RegInit(false.B)
    val aw_id = RegInit(0.U)
    val ar_id = RegInit(0.U)

    // handle aw/w->b begin
    when(in.aw.fire() && address.contains(in.aw.bits.addr) && in.w.fire())
    { 
      uart_reg := in.w.bits.data(31,0) 
      aw_fire := in.aw.fire()
      aw_id := in.aw.bits.id
    }
    in.b.valid := aw_fire
    in.b.bits.id := aw_id
    when(aw_fire && in.b.fire())
    { aw_fire := false.B }
    // handle aw/w->b end

    // handle ar->r begin
    val sel_s0 = address.contains(in.ar.bits.addr); chisel3.dontTouch(sel_s0)
    when(in.ar.fire() && address.contains(in.ar.bits.addr))
    { 
      ar_fire := true.B
      ar_id := in.aw.bits.id
    }
    in.r.valid := ar_fire
    in.r.bits.data := ar_id
    in.r.bits.data := Cat(0.U(33.W), uart_reg(30,0))
    when(ar_fire && in.r.fire())
    { ar_fire := false.B }
    // handle ar->r end

  }
}


class Axi4Tcam(id: Int)(implicit p: Parameters) extends LazyModule with NpusParams
{
  val address = AddressSet(tcamBase, tcamSize-1)
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(address),
      //resources     = resources,
      regionType    = if (true) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = true,
      supportsRead  = TransferSizes(1, fetchBytes),
      supportsWrite = TransferSizes(1, fetchBytes),
      interleavedId = Some(0))),
    beatBytes  = fetchBytes,
    requestKeys = if (true) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  lazy val module = new LazyModuleImp(this) 
  {
    val (in, edgeIn) = node.in(0)
    chisel3.dontTouch(in)
    val tcam_reg = RegInit(0x72345678.U(32.W)); chisel3.dontTouch(tcam_reg)
    in.aw.ready := true.B
    in.w.ready := true.B
    in.ar.ready := true.B
    
    val aw_fire = RegInit(false.B)
    val ar_fire = RegInit(false.B)
    val aw_id = RegInit(0.U)
    val ar_id = RegInit(0.U)

    val IncCounter = Counter(5)

    // handle aw/w->b begin
    when(in.aw.fire() && address.contains(in.aw.bits.addr) && in.w.fire())
    { 
      tcam_reg := in.w.bits.data(31,0) 
      aw_fire := in.aw.fire()
      aw_id := in.aw.bits.id
    }
    in.b.valid := aw_fire
    in.b.bits.id := aw_id
    when(aw_fire && in.b.fire())
    { aw_fire := false.B }
    // handle aw/w->b end

    // handle ar->r begin
    val sel_s0 = address.contains(in.ar.bits.addr); chisel3.dontTouch(sel_s0)
    when(in.ar.fire() && address.contains(in.ar.bits.addr))
    { 
      ar_fire := true.B
      ar_id := in.aw.bits.id
      IncCounter.reset
    }

    in.r.valid := ar_fire && IncCounter.inc
    in.r.bits.data := ar_id
    in.r.bits.data := Cat(0.U(33.W), tcam_reg(30,0))
    when(ar_fire && in.r.fire())
    { ar_fire := false.B }
    // handle ar->r end

  }
}

class Axi4Lram(id: Int)(implicit p: Parameters) extends LazyModule with NpusParams
{
  val address = AddressSet(lramBase, lramSize-1)
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(address),
      //resources     = resources,
      regionType    = if (true) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = true,
      supportsRead  = TransferSizes(1, fetchBytes),
      supportsWrite = TransferSizes(1, fetchBytes),
      interleavedId = Some(0))),
    beatBytes  = fetchBytes,
    requestKeys = if (true) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  lazy val module = new LazyModuleImp(this) 
  {
    val (in, edgeIn) = node.in(0)
    chisel3.dontTouch(in)
    val lram_reg = RegInit(0x72345678.U(32.W)); chisel3.dontTouch(lram_reg)
    in.aw.ready := true.B
    in.w.ready := true.B
    in.ar.ready := true.B
    
    val aw_fire = RegInit(false.B)
    val ar_fire = RegInit(false.B)
    val aw_id = RegInit(0.U)
    val ar_id = RegInit(0.U)

    val IncCounter = Counter(5)

    // handle aw/w->b begin
    when(in.aw.fire() && address.contains(in.aw.bits.addr) && in.w.fire())
    { 
      lram_reg := in.w.bits.data(31,0) 
      aw_fire := in.aw.fire()
      aw_id := in.aw.bits.id
    }
    in.b.valid := aw_fire
    in.b.bits.id := aw_id
    when(aw_fire && in.b.fire())
    { aw_fire := false.B }
    // handle aw/w->b end

    // handle ar->r begin
    val sel_s0 = address.contains(in.ar.bits.addr); chisel3.dontTouch(sel_s0)
    when(in.ar.fire() && address.contains(in.ar.bits.addr))
    { 
      ar_fire := true.B
      ar_id := in.aw.bits.id
      IncCounter.reset
    }

    in.r.valid := ar_fire && IncCounter.inc
    in.r.bits.data := ar_id
    in.r.bits.data := Cat(0.U(33.W), lram_reg(30,0))
    when(ar_fire && in.r.fire())
    { ar_fire := false.B }
    // handle ar->r end

  }
}



