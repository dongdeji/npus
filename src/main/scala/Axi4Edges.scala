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
     io.bits match { case aw: AXI4BundleAW => WireInit(0.U)
                     case  w: AXI4BundleW  => w.data
                     case  b: AXI4BundleB  => WireInit(0.U)
                     case ar: AXI4BundleAR => WireInit(0.U)
                     case  r: AXI4BundleR  => r.data } }

  def axi4strb[T <: AXI4BundleBase](io: IrrevocableIO[T]) = {
     io.bits match { case aw: AXI4BundleAW => WireInit(0.U)
                     case  w: AXI4BundleW  => w.strb
                     case  b: AXI4BundleB  => WireInit(0.U)
                     case ar: AXI4BundleAR => WireInit(0.U)
                     case  r: AXI4BundleR  => WireInit(0.U) } }

  def axi4id[T <: AXI4BundleBase](io: IrrevocableIO[T]) = {
     io.bits match { case aw: AXI4BundleAW => aw.id
                     case  w: AXI4BundleW  => WireInit(0.U)
                     case  b: AXI4BundleB  => WireInit(0.U)
                     case ar: AXI4BundleAR => ar.id
                     case  r: AXI4BundleR  => WireInit(0.U) } }

  def axi4hasData[T <: AXI4BundleBase](io: IrrevocableIO[T]) = {
     io.bits match { case aw: AXI4BundleAW => false
                     case  w: AXI4BundleW  => true
                     case  b: AXI4BundleB  => false
                     case ar: AXI4BundleAR => false
                     case  r: AXI4BundleR  => true } }
}



