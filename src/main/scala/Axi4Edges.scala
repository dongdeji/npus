// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util._

import chisel3._
import chisel3.util.{DecoupledIO, log2Ceil, Cat, RegEnable}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.axi4._

sealed trait AXI4Channel extends AXI4BundleBase {
  val channelName: String
}

sealed trait AXI4DataChannel extends AXI4Channel

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
