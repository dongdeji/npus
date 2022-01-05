// See LICENSE.SiFive for license details.

package npus

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.axi4._
import chisel3.experimental.chiselName

import AXI4EdgeUtil._

// innBeatBytes => the new client-facing bus width
//@chiselName
class AXI4WidthWidget_v0p1(innerBeatBytes: Int)(implicit p: Parameters) extends LazyModule
{
  val node = AXI4AdapterNode(
              masterFn = { mp => mp },
              slaveFn  = { sp => 
                sp.copy(beatBytes = innerBeatBytes, 
                        slaves = sp.slaves.map { s =>  
                          s.copy( supportsWrite = s.supportsWrite.intersect(
                                    TransferSizes(1, innerBeatBytes*(1 << AXI4Parameters.lenBits))),
                                  supportsRead =  s.supportsRead.intersect(
                                    TransferSizes(1, innerBeatBytes*(1 << AXI4Parameters.lenBits))) )})
                          } )

  lazy val module = new LazyModuleImp(this) {

    class AXI4Meta(edge: AXI4EdgeParameters) extends Bundle
    {
      val id     = UInt(edge.bundle.idBits.W)
      val addr   = UInt(edge.bundle.addrBits.W)
      val len    = UInt(edge.bundle.lenBits.W)  // number of beats - 1
      val size   = UInt(edge.bundle.sizeBits.W) // bytes in beat = 2^size

      override def cloneType: this.type = (new AXI4Meta(edge)).asInstanceOf[this.type]
    }

    def merge[T <: AXI4BundleBase](edgeIn: AXI4EdgeParameters,  in: IrrevocableIO[T], metaIn: AXI4Meta, 
                                  edgeOut: AXI4EdgeParameters, out: IrrevocableIO[T]) = {
      require( edgeIn.slave.beatBytes < edgeOut.slave.beatBytes )
      val inBytes = edgeIn.slave.beatBytes
      val outBytes = edgeOut.slave.beatBytes
      val ratio = outBytes / inBytes
      val keepBits  = log2Ceil(outBytes)
      val dropBits  = log2Ceil(inBytes)
      val countBits = log2Ceil(ratio)

      val hasData = axi4hasData(in)
      val limit   = metaIn.len

      val count  = RegInit(0.U(countBits.W))
      val first  = count === 0.U
      val last   = count === limit || !hasData.B
      val enable = Seq.tabulate(ratio) { i => !((count ^ i.U) & limit).orR }

      when (in.fire()) {
        count := count + 1.U
        when (last) { count := 0.U }
      }

      val index = (count + (metaIn.addr >> dropBits))(countBits-1, 0)
      val rdata_R = Reg(Vec(ratio, chiselTypeOf(axi4data(in))))
      val rdata_W = Wire(Vec(ratio, chiselTypeOf(axi4data(in))))
      val rstrb_R = Reg(Vec(ratio, chiselTypeOf(axi4strb(in))))
      val rstrb_W = Wire(Vec(ratio, chiselTypeOf(axi4strb(in))))
      rdata_W := rdata_R
      rstrb_W := rstrb_R
      when(in.fire()) { rdata_R(index) := axi4data(in); rdata_W(index) := axi4data(in) }
      when(in.fire()) { rstrb_R(index) := axi4strb(in); rstrb_W(index) := axi4strb(in) }

      in.ready := out.ready || !last
      out.valid := in.valid && last
      out.bits := in.bits
      axi4data(out) := rdata_W.asUInt
      axi4strb(out) := rstrb_W.asUInt
    } // end of merge

    def split[T <: AXI4BundleBase]( edgeIn: AXI4EdgeParameters,  in: IrrevocableIO[T],  metaIn: AXI4Meta, 
                                   edgeOut: AXI4EdgeParameters, out: IrrevocableIO[T] ) = {
      require(edgeIn.slave.beatBytes > edgeOut.slave.beatBytes)
      val inBytes = edgeIn.slave.beatBytes
      val outBytes = edgeOut.slave.beatBytes
      val ratio = inBytes / outBytes
      val keepBits = log2Ceil(inBytes)
      val dropBits  = log2Ceil(outBytes)
      val countBits = log2Ceil(ratio)

      val size    = metaIn.size
      val hasData = axi4hasData(in) // to do by dongdeji
      val limit   = UIntToOH1(size, keepBits) >> dropBits

      val count = RegInit(0.U(countBits.W))
      val first = count === 0.U
      val last  = count === limit || !(hasData.B)

      when (out.fire()) {
        count := count + 1.U
        when (last) { count := 0.U }
      }

      val dataIndex = (count + (metaIn.addr >> dropBits))(countBits-1, 0)
      def helper(idata: UInt, width: Int): UInt = {
        val mux = VecInit.tabulate(ratio) { i => idata((i+1)*outBytes*width-1, i*outBytes*width) }
        mux(dataIndex)
      }      

      out.bits := in.bits
      out.valid := in.valid
      in.ready := out.ready

      axi4data(out) := helper(axi4data(in), 8)
      axi4strb(out) := helper(axi4strb(in), 1)
      axi4last(out) := axi4last(in) && last

      !last  // lock NpRepeater 
    } // end of split
    
    def splice[T <: AXI4BundleBase]( edgeIn: AXI4EdgeParameters,  in: IrrevocableIO[T], metaIn: AXI4Meta, 
                                    edgeOut: AXI4EdgeParameters, out: IrrevocableIO[T] ) = {  
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

        axi4strb(cated) := Cat(
              axi4strb(repeated)(edgeIn.slave.beatBytes-1, edgeOut.slave.beatBytes),
              axi4strb(in)(edgeOut.slave.beatBytes-1, 0))

        axi4data(cated) := Cat(
              axi4data(repeated)(edgeIn.slave.beatBytes*8-1, edgeOut.slave.beatBytes*8),
              axi4data(in)(edgeOut.slave.beatBytes*8-1, 0))

        repeat := split(edgeIn, cated, metaIn, edgeOut, out)
      } else {
        // merge input to output
        merge(edgeIn, in, metaIn, edgeOut, out)
      }
    } // end of splice

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>

      // If the master is narrower than the slave, the D channel must be narrowed.
      // This is tricky, because the D channel has no address data.
      // Thus, you don't know which part of a sub-beat transfer to extract.
      // To fix this, we record the relevant address bits for all sources.
      // The assumption is that this sort of situation happens only where
      // you connect a narrow master to the system bus, so there are few sources.

      val awid = axi4id(in.aw)
      val awidHold = Mux(in.aw.fire(), awid, RegEnable(awid, in.aw.fire()))
      val arid = axi4id(in.ar)
      val aridHold = Mux(in.ar.fire(), arid, RegEnable(arid, in.ar.fire()))

      def getAWMetaR(id: UInt): AXI4Meta = {
        val awmeta_R = Reg(Vec(edgeIn.master.endId, new AXI4Meta(edgeIn)))
        val awmeta   = Wire(Vec(edgeIn.master.endId, new AXI4Meta(edgeIn)))

        awmeta(in.aw.bits.id).id   := Mux(in.ar.fire(), in.aw.bits.id  , RegEnable(in.aw.bits.id  , in.ar.fire()))
        awmeta(in.aw.bits.id).addr := Mux(in.ar.fire(), in.aw.bits.addr, RegEnable(in.aw.bits.addr, in.ar.fire()))
        awmeta(in.aw.bits.id).len  := Mux(in.ar.fire(), in.aw.bits.len , RegEnable(in.aw.bits.len , in.ar.fire()))
        awmeta(in.aw.bits.id).size := Mux(in.ar.fire(), in.aw.bits.id  , RegEnable(in.aw.bits.size, in.ar.fire()))

        awmeta(id)
      }

      def getARMetaR(id: UInt): AXI4Meta = {
        val armeta  = Reg(Vec(edgeIn.master.endId, new AXI4Meta(edgeIn)))
        val shift = log2Ceil(edgeIn.slave.beatBytes) - log2Ceil(edgeOut.slave.beatBytes)
        val len = if(shift >= 0) in.ar.bits.len << shift else in.ar.bits.len >> shift.abs
        val size = if(shift >= 0) in.ar.bits.size >> shift else in.ar.bits.size << shift.abs
        when (in.ar.fire()) {
          armeta(in.ar.bits.id).id   := in.ar.bits.id
          armeta(in.ar.bits.id).addr := in.ar.bits.addr
          armeta(in.ar.bits.id).len  := len
          assert( (in.ar.bits.len >> (in.ar.bits.len.getWidth - shift)) === 0.U )
          armeta(in.ar.bits.id).size := size
        }
        armeta(id)
      }

      splice(edgeIn ,  in.aw, getAWMetaR(awidHold), edgeOut, out.aw)
      splice(edgeIn ,  in.w , getAWMetaR(awidHold), edgeOut, out.w )
      splice(edgeOut, out.b , getAWMetaR(awidHold), edgeIn , in.b  )
      splice(edgeIn ,  in.ar, getARMetaR(aridHold), edgeOut, out.ar )
      splice(edgeOut, out.r , getARMetaR(aridHold), edgeIn , in.r  )

    }
  }
}

object AXI4WidthWidget
{
  def apply(innerBeatBytes: Int)(implicit p: Parameters): AXI4Node =
  {
    val axi4widget = LazyModule(new AXI4WidthWidget_v0p1(innerBeatBytes))
    axi4widget.node
  }
}


