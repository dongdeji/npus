// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

//import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util._

import chisel3._
import chisel3.util.{IrrevocableIO, DecoupledIO, log2Ceil, Cat, RegEnable}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.axi4._

class AXI4Edge(
  master: AXI4MasterPortParameters,
  slave:  AXI4SlavePortParameters,
  params: Parameters,
  sourceInfo: SourceInfo)
  extends AXI4EdgeParameters(master, slave, params, sourceInfo)
{

}

class AXI4EdgeOut(
  master: AXI4MasterPortParameters,
  slave:  AXI4SlavePortParameters,
  params: Parameters,
  sourceInfo: SourceInfo)
  extends AXI4Edge(master, slave, params, sourceInfo)
{

}

class TLEdgeIn(
  master: AXI4MasterPortParameters,
  slave:  AXI4SlavePortParameters,
  params: Parameters,
  sourceInfo: SourceInfo)
  extends AXI4Edge(master, slave, params, sourceInfo)
{

}

object AXI4EdgeUtil {

  def axi4data[T <: AXI4BundleBase](io: IrrevocableIO[T]) = {
     io.bits match { case aw: AXI4BundleAW => WireInit(0.U(aw.params.dataBits.W))
                     case  w: AXI4BundleW  => w.data
                     case  b: AXI4BundleB  => WireInit(0.U(b.params.dataBits.W))
                     case ar: AXI4BundleAR => WireInit(0.U(ar.params.dataBits.W))
                     case  r: AXI4BundleR  => r.data } }

  def axi4strb[T <: AXI4BundleBase](io: IrrevocableIO[T]) = {
     io.bits match { case aw: AXI4BundleAW => WireInit(0.U((aw.params.dataBits/8).W))
                     case  w: AXI4BundleW  => w.strb
                     case  b: AXI4BundleB  => WireInit(0.U((b.params.dataBits/8).W))
                     case ar: AXI4BundleAR => WireInit(0.U((ar.params.dataBits/8).W))
                     case  r: AXI4BundleR  => WireInit(0.U((r.params.dataBits/8).W)) } }

  def axi4id[T <: AXI4BundleBase](io: IrrevocableIO[T]) = {
     io.bits match { case aw: AXI4BundleAW => aw.id
                     case  w: AXI4BundleW  => WireInit(0.U(w.params.idBits.W))
                     case  b: AXI4BundleB  => WireInit(0.U(b.params.idBits.W))
                     case ar: AXI4BundleAR => ar.id
                     case  r: AXI4BundleR  => WireInit(0.U(r.params.idBits.W)) } }

  def axi4last[T <: AXI4BundleBase](io: IrrevocableIO[T]) = {
     io.bits match { case aw: AXI4BundleAW => WireInit(false.B)
                     case  w: AXI4BundleW  => w.last
                     case  b: AXI4BundleB  => WireInit(false.B)
                     case ar: AXI4BundleAR => WireInit(false.B)
                     case  r: AXI4BundleR  => r.last } }

  def axi4size[T <: AXI4BundleBase](io: IrrevocableIO[T]) = {
     io.bits match { case aw: AXI4BundleAW => aw.size
                     case  w: AXI4BundleW  => WireInit(0.U(w.params.sizeBits.W))
                     case  b: AXI4BundleB  => WireInit(0.U(b.params.sizeBits.W))
                     case ar: AXI4BundleAR => ar.size
                     case  r: AXI4BundleR  => WireInit(0.U(r.params.sizeBits.W)) } }

  def axi4hasData[T <: AXI4BundleBase](io: IrrevocableIO[T]) = {
     io.bits match { case aw: AXI4BundleAW => false
                     case  w: AXI4BundleW  => true
                     case  b: AXI4BundleB  => false
                     case ar: AXI4BundleAR => false
                     case  r: AXI4BundleR  => true } }
}



