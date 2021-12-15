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


object NpuALU
{
  val SZ_ALU_FN = 4
  def FN_X    = BitPat("b????")
  def FN_ADD  = 0.U
  def FN_SL   = 1.U
  def FN_SEQ  = 2.U
  def FN_SNE  = 3.U
  def FN_XOR  = 4.U
  def FN_SR   = 5.U
  def FN_OR   = 6.U
  def FN_AND  = 7.U
  def FN_SUB  = 10.U
  def FN_SRA  = 11.U
  def FN_SLT  = 12.U
  def FN_SGE  = 13.U
  def FN_SLTU = 14.U
  def FN_SGEU = 15.U

  def FN_DIV  = FN_XOR
  def FN_DIVU = FN_SR
  def FN_REM  = FN_OR
  def FN_REMU = FN_AND

  def FN_MUL    = FN_ADD
  def FN_MULH   = FN_SL
  def FN_MULHSU = FN_SEQ
  def FN_MULHU  = FN_SNE

  def isMulFN(fn: UInt, cmp: UInt) = fn(1,0) === cmp(1,0)
  def isSub(cmd: UInt) = cmd(3)
  def isCmp(cmd: UInt) = cmd >= FN_SLT
  def cmpUnsigned(cmd: UInt) = cmd(1)
  def cmpInverted(cmd: UInt) = cmd(0)
  def cmpEq(cmd: UInt) = !cmd(3)
}

import NpuALU._

/**
 * Abstract trait giving defaults and other relevant values to different Decode constants/
 */
abstract trait DecodeConstants {
  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")

  /*def decodeDefault: List[BitPat] = // illegal instruction
    //   srcType(0)     srcType(1)     srcType(2)     fuType      fuOpType    rfWen
    //   |            |            |            |           |           |  fpWen
    //   |            |            |            |           |           |  |  isXSTrap
    //   |            |            |            |           |           |  |  |  noSpecExec
    //   |            |            |            |           |           |  |  |  |  blockBackward
    //   |            |            |            |           |           |  |  |  |  |  flushPipe
    //   |            |            |            |           |           |  |  |  |  |  |  isRVF
    //   |            |            |            |           |           |  |  |  |  |  |  |  selImm
    List(SrcType.DC, SrcType.DC, SrcType.DC, FuType.alu, ALUOpType.sll, N, N, N, N, N, N, N, SelImm.INVALID_INSTR) // Use SelImm to indicate invalid instr
  */
    val table: Array[(BitPat, List[BitPat])]
}


class InstrCtrlSigs extends Bundle {
  val legal = Bool()
  val fp = Bool()
  val custom = Bool()
  val branch = Bool()
  val jal = Bool()
  val jalr = Bool()
  val rxs2 = Bool()
  val rxs1 = Bool()
  val scie = Bool()
  val sel_alu2 = Bits(A2_X.getWidth.W)
  val sel_alu1 = Bits(A1_X.getWidth.W)
  val sel_imm = Bits(IMM_X.getWidth.W)
  val alu_dw = Bool()
  val alu_fn = Bits(FN_X.getWidth.W)
  val mem = Bool()
  val mem_cmd = Bits(M_SZ.W)
  val rfs1 = Bool()
  val rfs2 = Bool()
  val rfs3 = Bool()
  val wfd = Bool()
  val mul = Bool()
  val div = Bool()
  val wxd = Bool()
  val csr = Bits(CSR.SZ.W)
  val fence_i = Bool()
  val fence = Bool()
  val amo = Bool()
  val dp = Bool()

  def default: List[BitPat] =
                //           jal                                                             renf1               fence.i
                //   val     | jalr                                                          | renf2             |
                //   |fp_val | | renx2                                                       | | renf3           |
                //   | custom| | | renx1       s_alu1                          mem_val       | | | wfd           |
                //   | | | br| | | |   s_alu2  |       imm    dw     alu       | mem_cmd     | | | | mul         |
                //   | | | | | | | |   |       |       |      |      |         | |           | | | | | div       | fence
                //   | | | | | | | |   |       |       |      |      |         | |           | | | | | | wxd     | | amo
                //   | | | | | | | | scie      |       |      |      |         | |           | | | | | | |       | | | dp
                List(N,X,X,X,X,X,X,X,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        X,X,X,X,X,X,X,CSR.X,X,X,X,X)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, fp, custom, branch, jal, jalr, rxs2, rxs1, scie, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd,
                   rfs1, rfs2, rfs3, wfd, mul, div, wxd, csr, fence_i, fence, amo, dp)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class IDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    BNE->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    BEQ->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    BLT->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    BLTU->      List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    BGE->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    BGEU->      List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

    JAL->       List(Y,N,N,N,Y,N,N,N,N,A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    JALR->      List(Y,N,N,N,N,Y,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    AUIPC->     List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),

    LB->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    LH->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    LW->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    LBU->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    LHU->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SB->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    SH->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    SW->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N),

    LUI->       List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    ADDI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLTI ->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLTIU->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    ANDI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    ORI->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    XORI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    ADD->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SUB->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLT->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLTU->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    AND->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    OR->        List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    XOR->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLL->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRL->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRA->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),

    FENCE->     List(Y,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.N,N,Y,N,N),

    SCALL->     List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N),
    SBREAK->    List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N),
    MRET->      List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N),
    WFI->       List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N),
    CEASE->     List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N),
    CSRRW->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N),
    CSRRS->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N),
    CSRRC->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N),
    CSRRWI->    List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N),
    CSRRSI->    List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N),
    CSRRCI->    List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N))
}


class I64Decode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    LD->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    LWU->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SD->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N),

    SLLI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRLI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRAI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),

    ADDIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLLIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRLIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRAIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    ADDW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SUBW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SUB,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLLW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRLW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRAW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N))
}


class CUSTOMDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CUSTOM0->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM0_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM0_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM0_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM0_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM0_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM1->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM1_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM1_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM1_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM1_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM1_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM2->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM2_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM2_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM2_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM2_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM2_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM3->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM3_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM3_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM3_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM3_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM3_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N))
}



class NpuALU extends Module with NpusParams
{
  val io = IO(new Bundle {
                  val dw = Input(UInt(SZ_DW.W)) //Bits(INPUT, SZ_DW)
                  val fn = Input(UInt(SZ_ALU_FN.W)) //Bits(INPUT, SZ_ALU_FN)
                  val in2 = Input(UInt(dataWidth.W)) //UInt(INPUT, dataWidth)
                  val in1 = Input(UInt(dataWidth.W)) //UInt(INPUT, dataWidth)
                  val out = Output(UInt(dataWidth.W)) //UInt(OUTPUT, dataWidth)
                  val adder_out = Output(UInt(dataWidth.W)) //UInt(OUTPUT, dataWidth)
                  val cmp_out = Output(Bool()) //Bool(OUTPUT)
                })

  // ADD, SUB
  val in2_inv = Mux(isSub(io.fn), ~io.in2, io.in2)
  val in1_xor_in2 = io.in1 ^ in2_inv
  io.adder_out := io.in1 + in2_inv + isSub(io.fn)

  // SLT, SLTU
  val slt = Mux(io.in1(dataWidth-1) === io.in2(dataWidth-1), io.adder_out(dataWidth-1),
                  Mux(cmpUnsigned(io.fn), io.in2(dataWidth-1), io.in1(dataWidth-1)))
                  io.cmp_out := cmpInverted(io.fn) ^ Mux(cmpEq(io.fn), in1_xor_in2 === 0.U, slt)

  // SLL, SRL, SRA
  val (shamt, shin_r) =
  if (dataWidth == 32) (io.in2(4,0), io.in1)
  else {
    require(dataWidth == 64)
    val shin_hi_32 = Fill(32, isSub(io.fn) && io.in1(31))
    val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
    val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0))
    (shamt, Cat(shin_hi, io.in1(31,0)))
  }

  val shin = Mux(io.fn === FN_SR || io.fn === FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(isSub(io.fn) & shin(dataWidth-1), shin).asSInt >> shamt)(dataWidth-1,0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(io.fn === FN_SR || io.fn === FN_SRA, shout_r, 0.U) |
                              Mux(io.fn === FN_SL, shout_l, 0.U)
  // AND, OR, XOR
  val logic = Mux(io.fn === FN_XOR || io.fn === FN_OR, in1_xor_in2, 0.U) |
                        Mux(io.fn === FN_OR || io.fn === FN_AND, io.in1 & io.in2, 0.U)
  val shift_logic = (isCmp(io.fn) && slt) | logic | shout
  val out = Mux(io.fn === FN_ADD || io.fn === FN_SUB, io.adder_out, shift_logic)

  io.out := out
  if (dataWidth > 32) {
    require(dataWidth == 64)
    when (io.dw === DW_32) { io.out := Cat(Fill(32, out(31)), out(31,0)) }
  }
}


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

class ThreadUop extends Bundle with NpusParams {
  val ready = Bool()
  val valid = Bool() // valid after decode
  val tid = UInt(log2Up(numThread).W)
  val ctrl = new InstrCtrlSigs
  val pc = UInt(pcWidth.W) // valid after decode
  val instr = Bits(instrWidth.W) // valid after decode
  val rd_valid = Bool() // valid after alu/lsu/mul/div/fpu
  val rd = UInt((log2Up(numThread)+5).W) // valid after decode, bit40 for register index, others for tid
  val rs1 = UInt((log2Up(numThread)+5).W) // valid after decode, bit40 for register index, others for tid
  val rs2 = UInt((log2Up(numThread)+5).W) // valid after decode, bit4~0 for register index, others for tid
  val rd_data = Bits(dataWidth.W) // valid after alu/lsu/mul/div/fpu
  val rs1_data = Bits(dataWidth.W) // valid at the end of ISSUE
  val rs2_data = Bits(dataWidth.W) // valid at the end of ISSUE
  val resped = Bool() // mark mem/custom responed
}

class Core(implicit p: Parameters) extends LazyModule with NpusParams 
{
  val masternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                      masters = Seq(AXI4MasterParameters(
                                                      name = s"Core",
                                                      id   = IdRange(0, 1 << 1))))))
  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val ctrl = Output(new Bundle { 
          val thread_readys = Vec(numThread, Bool()) } 
      )
      val redirect = Output(Valid( new Bundle {
          val tid = UInt(tidWidth.W)
          val npc = UInt(pcWidth.W) }
      ))
      val instr = Input(Valid( new Bundle {
          val tid = UInt(tidWidth.W)
          val pc = UInt(pcWidth.W)
          val instr = UInt(instrWidth.W) }
      ))
    })
    chisel3.dontTouch(io)
    val (out, edge) = masternode.out(0)

    /**********************************************************/
    /****************** decode stage begin ********************/
    val decode_table = { Seq(new CUSTOMDecode) ++: Seq(new I64Decode) ++: Seq(new IDecode) } flatMap(_.table)
    val id_ctrl = Wire(new InstrCtrlSigs()).decode(io.instr.bits.instr, decode_table); chisel3.dontTouch(id_ctrl)
    val id_rs1 = WireInit(Mux(id_ctrl.rxs1, io.instr.bits.instr(19,15), 0.U(5.W)))
    val id_rs2 = WireInit(Mux(id_ctrl.rxs2, io.instr.bits.instr(24,20), 0.U(5.W)))

    /****************** decode stage begin ********************/
    /**********************************************************/
    val rr_uops = RegInit(VecInit(Seq.fill(3)(0.U.asTypeOf(new ThreadUop))));rr_uops.foreach(dontTouch(_))
    val ex_uop = RegInit(0.U.asTypeOf(new ThreadUop));dontTouch(ex_uop)
    val wb_uop = RegInit(0.U.asTypeOf(new ThreadUop));dontTouch(wb_uop)

  }
}







