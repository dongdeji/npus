// See LICENSE.SiFive for license details.

package npus

import chisel3._
import chisel3.util.{IrrevocableIO, DecoupledIO, log2Ceil, Cat, RegEnable}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.axi4._

// innBeatBytes => the new client-facing bus width
class AXI4WidthWidget(innerBeatBytes: Int)(implicit p: Parameters) extends LazyModule
{
  //private def noChangeRequired(manager: TLManagerPortParameters) = manager.beatBytes == innerBeatBytes
  val node = AXI4AdapterNode(
               masterFn = { p => p },
               slaveFn  = { p => p.copy(beatBytes = innerBeatBytes) })

  lazy val module = new LazyModuleImp(this) {

    class AXI4Meta(edge: AXI4EdgeParameters) extends Bundle
    {
      val addr   = UInt(edge.bundle.addrBits.W)
      val len    = UInt(edge.bundle.lenBits.W)  // number of beats - 1
      val size   = UInt(edge.bundle.sizeBits.W) // bytes in beat = 2^size
    }

    //def split[T <: TLDataChannel](edgeIn: TLEdge, in: DecoupledIO[T], edgeOut: TLEdge, out: DecoupledIO[T], sourceMap: UInt => UInt) = {
    def merge[T <: AXI4BundleBase](edgeIn: AXI4EdgeParameters, in: IrrevocableIO[T], 
                                  edgeOut: AXI4EdgeParameters, out: IrrevocableIO[T]) = {

      val inBytes = edgeIn.slave.beatBytes
      val outBytes = edgeOut.slave.beatBytes
      val ratio = outBytes / inBytes
      val keepBits  = log2Ceil(outBytes)
      val dropBits  = log2Ceil(inBytes)
      val countBits = log2Ceil(ratio)

      /*val size    = edgeIn.size(in.bits)
      val hasData = edgeIn.hasData(in.bits)
      val limit   = UIntToOH1(size, keepBits) >> dropBits

      val count  = RegInit(0.U(countBits.W))
      val first  = count === 0.U
      val last   = count === limit || !hasData
      val enable = Seq.tabulate(ratio) { i => !((count ^ i.U) & limit).orR }

      val corrupt_reg = RegInit(false.B)
      val corrupt_in = edgeIn.corrupt(in.bits)
      val corrupt_out = corrupt_in || corrupt_reg

      when (in.fire()) {
        count := count + 1.U
        corrupt_reg := corrupt_out
        when (last) {
          count := 0.U
          corrupt_reg := false.B
        }
      }

      def helper(idata: UInt): UInt = {
        // rdata is X until the first time a multi-beat write occurs.
        // Prevent the X from leaking outside by jamming the mux control until
        // the first time rdata is written (and hence no longer X).
        val rdata_written_once = RegInit(false.B)
        val masked_enable = enable.map(_ || !rdata_written_once)

        val odata = Seq.fill(ratio) { WireInit(idata) }
        val rdata = Reg(Vec(ratio-1, chiselTypeOf(idata)))
        val pdata = rdata :+ idata
        val mdata = (masked_enable zip (odata zip pdata)) map { case (e, (o, p)) => Mux(e, o, p) }
        when (in.fire() && !last) {
          rdata_written_once := true.B
          (rdata zip mdata) foreach { case (r, m) => r := m }
        }
        Cat(mdata.reverse)
      }

      in.ready := out.ready || !last
      out.valid := in.valid && last
      out.bits := in.bits

      // Don't put down hardware if we never carry data
      edgeOut.data(out.bits) := (if (edgeIn.staticHasData(in.bits) == Some(false)) 0.U else helper(edgeIn.data(in.bits)))
      edgeOut.corrupt(out.bits) := corrupt_out

      (out.bits, in.bits) match {
        case (o: TLBundleA, i: TLBundleA) => o.mask := edgeOut.mask(o.address, o.size) & Mux(hasData, helper(i.mask), ~0.U(outBytes.W))
        case (o: TLBundleB, i: TLBundleB) => o.mask := edgeOut.mask(o.address, o.size) & Mux(hasData, helper(i.mask), ~0.U(outBytes.W))
        case (o: TLBundleC, i: TLBundleC) => ()
        case (o: TLBundleD, i: TLBundleD) => ()
        case _ => require(false, "Impossible bundle combination in WidthWidget")
      }*/
    }

    //def split[T <: TLDataChannel](edgeIn: TLEdge, in: DecoupledIO[T], edgeOut: TLEdge, out: DecoupledIO[T], sourceMap: UInt => UInt) = {
    def split[T <: AXI4BundleBase](edgeIn: AXI4EdgeParameters, in: IrrevocableIO[T], 
                                  edgeOut: AXI4EdgeParameters, out: IrrevocableIO[T], sourceMap: UInt => AXI4Meta) = {
      val inBytes = edgeIn.slave.beatBytes
      val outBytes = edgeOut.slave.beatBytes
      val ratio = inBytes / outBytes
      val keepBits  = log2Ceil(inBytes)
      val dropBits  = log2Ceil(outBytes)
      val countBits = log2Ceil(ratio)

      /*val size    = edgeIn.size(in.bits)
      val hasData = edgeIn.hasData(in.bits)
      val limit   = UIntToOH1(size, keepBits) >> dropBits*/

      val id = in.bits match {
        case aw: AXI4BundleAW => aw.id
        case ar: AXI4BundleAR => ar.id
      }

      val count = RegInit(0.U(countBits.W))
      val first = count === 0.U
      val holdId = Mux(first, id, RegEnable(id, first))
      //val last  = count === limit || !hasData
      val last  = false.B // to do by dongdeji

      /*when (out.fire()) {
        count := count + 1.U
        when (last) { count := 0.U }
      }

      // For sub-beat transfer, extract which part matters
      val sel = in.bits match {
        case a: TLBundleA => a.address(keepBits-1, dropBits)
        case b: TLBundleB => b.address(keepBits-1, dropBits)
        case c: TLBundleC => c.address(keepBits-1, dropBits)
        case d: TLBundleD => {
          val sel = sourceMap(d.source)
          val hold = Mux(first, sel, RegEnable(sel, first)) // a_first is not for whole xfer
          hold & ~limit // if more than one a_first/xfer, the address must be aligned anyway
        }
      }

      val index  = sel | count 
      def helper(idata: UInt, width: Int): UInt = {
        val mux = VecInit.tabulate(ratio) { i => idata((i+1)*outBytes*width-1, i*outBytes*width) }
        mux(index)
      }
      */

      out.bits := in.bits
      out.valid := in.valid
      in.ready := out.ready

      // Don't put down hardware if we never carry data
      /*
      edgeOut.data(out.bits) := (if (edgeIn.staticHasData(in.bits) == Some(false)) 0.U else helper(edgeIn.data(in.bits), 8))

      (out.bits, in.bits) match {
        case (o: TLBundleA, i: TLBundleA) => o.mask := helper(i.mask, 1)
        case (o: TLBundleB, i: TLBundleB) => o.mask := helper(i.mask, 1)
        case (o: TLBundleC, i: TLBundleC) => () // replicating corrupt to all beats is ok
        case (o: TLBundleD, i: TLBundleD) => ()
        case _ => require(false, "Impossbile bundle combination in WidthWidget")
      }
      */
      // Repeat the input if we're not last
      !last
    }
    
    //def splice[T <: TLDataChannel](edgeIn: AXI4Edge, in: DecoupledIO[T], edgeOut: AXI4Edge, out: DecoupledIO[T], sourceMap: UInt => UInt) = {
    //def fanout[T <: AXI4BundleBase](input: IrrevocableIO[T], select: Seq[Bool]) = {
    def splice[T <: AXI4BundleBase](edgeIn: AXI4EdgeParameters, in: IrrevocableIO[T], 
                                   edgeOut: AXI4EdgeParameters, out: IrrevocableIO[T], sourceMap: UInt => AXI4Meta) = {  
      if (edgeIn.slave.beatBytes == edgeOut.slave.beatBytes) {
        // nothing to do; pass it through
        out.bits := in.bits
        out.valid := in.valid
        in.ready := out.ready
      } else if (edgeIn.slave.beatBytes > edgeOut.slave.beatBytes) {
        // split input to output
        val repeat = Wire(Bool())
        val repeated = NpRepeater(in, repeat)
        val cated = Wire(chiselTypeOf(repeated))
        cated <> repeated
        
        def GetData(io: IrrevocableIO[T]) = {
           io.bits match { case w: AXI4BundleW => w.data
                           case r: AXI4BundleR => r.data } }

        GetData(cated) := Cat(
          GetData(repeated)(edgeIn.slave.beatBytes*8-1, edgeOut.slave.beatBytes*8),
          GetData(in)(edgeOut.slave.beatBytes*8-1, 0))
        /*edgeIn.data(cated.bits) := Cat(
          edgeIn.data(repeated.bits)(edgeIn.slave.beatBytes*8-1, edgeOut.slave.beatBytes*8),
          edgeIn.data(in.bits)(edgeOut.slave.beatBytes*8-1, 0))*/
        repeat := split(edgeIn, cated, edgeOut, out, sourceMap)
      } else {
        // merge input to output
        merge(edgeIn, in, edgeOut, out)
      }
    }

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>

      // If the master is narrower than the slave, the D channel must be narrowed.
      // This is tricky, because the D channel has no address data.
      // Thus, you don't know which part of a sub-beat transfer to extract.
      // To fix this, we record the relevant address bits for all sources.
      // The assumption is that this sort of situation happens only where
      // you connect a narrow master to the system bus, so there are few sources.

      def GetAWMetaR(id: UInt): AXI4Meta = {
        val meta  = Reg(Vec(edgeIn.master.endId, new AXI4Meta(edgeIn)))
        when (in.aw.fire()) {
          meta(in.aw.bits.id).addr := in.aw.bits.addr
          meta(in.aw.bits.id).len := in.aw.bits.len
          meta(in.aw.bits.id).size := in.aw.bits.size
        }
        meta(id)
      }
      def GetARMetaR(id: UInt): AXI4Meta = {
        val meta  = Reg(Vec(edgeIn.master.endId, new AXI4Meta(edgeIn)))
        when (in.ar.fire()) {
          meta(in.ar.bits.id).addr := in.ar.bits.addr
          meta(in.ar.bits.id).len := in.ar.bits.len
          meta(in.ar.bits.id).size := in.ar.bits.size
        }
        meta(id)
      }
      splice(edgeIn ,  in.aw, edgeOut, out.aw, GetAWMetaR)
      splice(edgeIn ,  in.w , edgeOut, out.w , GetAWMetaR)
      splice(edgeOut, out.b , edgeIn , in.b  , GetAWMetaR)
      splice(edgeOut, out.ar, edgeIn , in.ar , GetARMetaR)
      splice(edgeOut, out.r , edgeIn , in.r  , GetARMetaR)

    }
  }
}

object AXI4WidthWidget
{
  def apply(innerBeatBytes: Int)(implicit p: Parameters): AXI4Node =
  {
    val axi4widget = LazyModule(new AXI4WidthWidget(innerBeatBytes))
    axi4widget.node
  }
}

