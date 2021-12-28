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
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.tile._
import chisel3.experimental.chiselName


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
abstract trait DecodeConstants extends NpusParams {
  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")
  val table: Array[(BitPat, List[BitPat])]
  val MH = if (memInstrHalt) Y else N
}


class InstrCtrlSigs extends Bundle {
  val legal = Bool()
  val br = Bool()
  val jal = Bool()
  val jalr = Bool()
  val rxs2 = Bool()
  val rxs1 = Bool()
  val sel_alu2 = Bits(A2_X.getWidth.W)
  val sel_alu1 = Bits(A1_X.getWidth.W)
  val sel_imm = Bits(IMM_X.getWidth.W)
  val alu_dw = Bool()
  val alu_fn = Bits(FN_X.getWidth.W)
  val mem = Bool()
  val mem_cmd = Bits(M_SZ.W)
  val wxd = Bool()
  val csr = Bits(CSR.SZ.W)
  val mclk = Bool()
  val wind = Bool()
  val acc = Bool()

  def default: List[BitPat] =
                        // legal jal       sel_alu2        sel_imm                                 mclk
                        //   |   | jalr    |     sel_alu1  |      alu_dw                           | wind
                        //   |   | | rxs2  |       |       |      |      alu_fn  mem      wxd      | | acc
                        //   | br| | | rxs1|       |       |      |      |       | mem_cmd|  csr   | | | 
                        List(N,X,X,X,X,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   X,CSR.X, X,X,X)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, br, jal, jalr, rxs2, rxs1, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd, wxd, csr, mclk, wind, acc)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class IDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                        // legal jal       sel_alu2        sel_imm                                 mclk
                        //   |   | jalr    |     sel_alu1  |      alu_dw                           | wind
                        //   |   | | rxs2  |       |       |      |      alu_fn  mem      wxd      | | acc
                        //   | br| | | rxs1|       |       |      |      |       | mem_cmd|  csr   | | | 
    BNE->               List(Y,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE, N,M_X,   N,CSR.N, N,N,N),
    BEQ->               List(Y,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ, N,M_X,   N,CSR.N, N,N,N),
    BLT->               List(Y,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT, N,M_X,   N,CSR.N, N,N,N),
    BLTU->              List(Y,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,N,M_X,   N,CSR.N, N,N,N),
    BGE->               List(Y,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE, N,M_X,   N,CSR.N, N,N,N),
    BGEU->              List(Y,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,N,M_X,   N,CSR.N, N,N,N),

    JAL->               List(Y,N,Y,N,N,N,  A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,FN_ADD, N,M_X,   Y,CSR.N, N,N,N),
    JALR->              List(Y,N,N,Y,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X,   Y,CSR.N, N,N,N),
    AUIPC->             List(Y,N,N,N,N,N,  A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD, N,M_X,   Y,CSR.N, N,N,N),

    LB->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,CSR.N, N,N,N),
    LH->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,CSR.N, N,N,N),
    LW->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,CSR.N, N,N,N),
    LBU->               List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,CSR.N, N,N,N),
    LHU->               List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,CSR.N, N,N,N),
    SB->                List(Y,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD, Y,M_XWR, N,CSR.N, N,N,N),
    SH->                List(Y,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD, Y,M_XWR, N,CSR.N, N,N,N),
    SW->                List(Y,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD, Y,M_XWR, N,CSR.N, N,N,N),

    LUI->               List(Y,N,N,N,N,N,  A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD, N,M_X,   Y,CSR.N, N,N,N),
    ADDI->              List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X,   Y,CSR.N, N,N,N),
    SLTI ->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT, N,M_X,   Y,CSR.N, N,N,N),
    SLTIU->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,N,M_X,   Y,CSR.N, N,N,N),
    ANDI->              List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND, N,M_X,   Y,CSR.N, N,N,N),
    ORI->               List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,  N,M_X,   Y,CSR.N, N,N,N),
    XORI->              List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR, N,M_X,   Y,CSR.N, N,N,N),
    ADD->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD, N,M_X,   Y,CSR.N, N,N,N),
    SUB->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB, N,M_X,   Y,CSR.N, N,N,N),
    SLT->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT, N,M_X,   Y,CSR.N, N,N,N),
    SLTU->              List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,N,M_X,   Y,CSR.N, N,N,N),
    AND->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND, N,M_X,   Y,CSR.N, N,N,N),
    OR->                List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,  N,M_X,   Y,CSR.N, N,N,N),
    XOR->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR, N,M_X,   Y,CSR.N, N,N,N),
    SLL->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,  N,M_X,   Y,CSR.N, N,N,N),
    SRL->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,  N,M_X,   Y,CSR.N, N,N,N),
    SRA->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA, N,M_X,   Y,CSR.N, N,N,N),

    SCALL->             List(Y,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   N,CSR.I, N,N,N),
    SBREAK->            List(Y,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   N,CSR.I, N,N,N),
    MRET->              List(Y,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   N,CSR.I, N,N,N),
    WFI->               List(Y,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   N,CSR.I, N,N,N),
    CEASE->             List(Y,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   N,CSR.I, N,N,N),
    CSRRW->             List(Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD, N,M_X,   Y,CSR.W, N,N,N),
    CSRRS->             List(Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD, N,M_X,   Y,CSR.S, N,N,N),
    CSRRC->             List(Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD, N,M_X,   Y,CSR.C, N,N,N),
    CSRRWI->            List(Y,N,N,N,N,N,  A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD, N,M_X,   Y,CSR.W, N,N,N),
    CSRRSI->            List(Y,N,N,N,N,N,  A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD, N,M_X,   Y,CSR.S, N,N,N),
    CSRRCI->            List(Y,N,N,N,N,N,  A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD, N,M_X,   Y,CSR.C, N,N,N))
}


class I64Decode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                        // legal jal       sel_alu2        sel_imm                                 mclk
                        //   |   | jalr    |     sel_alu1  |      alu_dw                           | wind
                        //   |   | | rxs2  |       |       |      |      alu_fn  mem      wxd      | | acc
                        //   | br| | | rxs1|       |       |      |      |       | mem_cmd|  csr   | | | 
    LD->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,CSR.N, N,N,N),
    LWU->               List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,CSR.N, N,N,N),
    SD->                List(Y,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD, Y,M_XWR, N,CSR.N, N,N,N),
  
    SLLI->              List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,  N,M_X,   Y,CSR.N, N,N,N),
    SRLI->              List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,  N,M_X,   Y,CSR.N, N,N,N),
    SRAI->              List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA, N,M_X,   Y,CSR.N, N,N,N),
  
    ADDIW->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,  N,M_X,   Y,CSR.N, N,N,N),
    SLLIW->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_SL,   N,M_X,   Y,CSR.N, N,N,N),
    SRLIW->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_SR,   N,M_X,   Y,CSR.N, N,N,N),
    SRAIW->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_SRA,  N,M_X,   Y,CSR.N, N,N,N),
    ADDW->              List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_ADD,  N,M_X,   Y,CSR.N, N,N,N),
    SUBW->              List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_SUB,  N,M_X,   Y,CSR.N, N,N,N),
    SLLW->              List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_SL,   N,M_X,   Y,CSR.N, N,N,N),
    SRLW->              List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_SR,   N,M_X,   Y,CSR.N, N,N,N),
    SRAW->              List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_SRA,  N,M_X,   Y,CSR.N, N,N,N))
}



// | b31~b28     | b27~b25   | b24~b20  | b19~b15    | b14~12     | b11~b7 | b6~b0             |
// | funct4:0000 | func3:000 | imm[9:5] | imm[14:10] | imm[4:2]   | rd     | coustom0: 0001011 | -- acc load to pkt wind + jal
// | funct4:0000 | func3:001 | imm[9:5] | imm[14:10] | imm[4:2]   | rd     | coustom0: 0001011 | -- acc load to result wind + jal
// | funct4:0000 | func3:010 | imm[9:5] | imm[14:10] | imm[4:2]   | rd     | coustom0: 0001011 | -- acc load to rd + jal
// | funct4:0001 | func3:000 | imm[9:5] | rs1        | imm[4:2]   | rd     | coustom0: 0001011 | -- acc load to pkt wind + jalr
// | funct4:0001 | func3:001 | imm[9:5] | rs1        | imm[4:2]   | rd     | coustom0: 0001011 | -- acc load to result wind + jalr
// | funct4:0001 | func3:010 | imm[9:5] | rs1        | imm[4:2]   | rd     | coustom0: 0001011 | -- acc load to rd + jalr
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:000 | rd     | coustom0: 0001011 | -- swap pkt wind byte
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:001 | rd     | coustom0: 0001011 | -- swap pkt wind halfword
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:010 | rd     | coustom0: 0001011 | -- swap pkt wind word
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:011 | rd     | coustom0: 0001011 | -- swap pkt wind doubleword
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:100 | rd     | coustom0: 0001011 | -- swap pkt wind unsigned byte
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:101 | rd     | coustom0: 0001011 | -- swap pkt wind unsigned halfword
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size:110 | rd     | coustom0: 0001011 | -- swap pkt wind unsigned word
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:000 | rd     | coustom0: 0001011 | -- swap result wind byte
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:001 | rd     | coustom0: 0001011 | -- swap result wind halfword
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:010 | rd     | coustom0: 0001011 | -- swap result wind word
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:011 | rd     | coustom0: 0001011 | -- swap result wind doubleword
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:100 | rd     | coustom0: 0001011 | -- swap result wind unsigned byte
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:101 | rd     | coustom0: 0001011 | -- swap result wind unsigned halfword
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:110 | rd     | coustom0: 0001011 | -- swap result wind unsigned word
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size:110 | rd | coustom0: 0001011 | -- swap result wind unsigned word

/* Automatically generated by parse-opcodes */
object NpInstructions {
  def LPKTWJAL          = BitPat("b0000000??????????????????0001011")
  def LRESWJAL          = BitPat("b0000001??????????????????0001011")
  def LRDJAL            = BitPat("b0000010??????????????????0001011")
  def LPKTWJALR         = BitPat("b0001000??????????????????0001011")
  def LRESWJALR         = BitPat("b0001001??????????????????0001011")
  def LRDJALR           = BitPat("b0001010??????????????????0001011")
  def SWAPPKTB          = BitPat("b0010000??????????000?????0001011")
  def SWAPPKTH          = BitPat("b0010000??????????001?????0001011")
  def SWAPPKTW          = BitPat("b0010000??????????010?????0001011")
  def SWAPPKTD          = BitPat("b0010000??????????011?????0001011")
  def SWAPPKTUB         = BitPat("b0010000??????????100?????0001011")
  def SWAPPKTUH         = BitPat("b0010000??????????101?????0001011")
  def SWAPPKTUW         = BitPat("b0010000??????????110?????0001011")
  def SWAPRESB          = BitPat("b0010001??????????000?????0001011")
  def SWAPRESH          = BitPat("b0010001??????????001?????0001011")
  def SWAPRESW          = BitPat("b0010001??????????010?????0001011")
  def SWAPRESD          = BitPat("b0010001??????????011?????0001011")
  def SWAPRESUB         = BitPat("b0010001??????????100?????0001011")
  def SWAPRESUH         = BitPat("b0010001??????????101?????0001011")
  def SWAPRESUW         = BitPat("b0010001??????????110?????0001011")
}

object NpInstrImmGenOld {
  def apply(inst: UInt) = {
    val sign = Mux(inst(31, 28) === 0.U, inst(19), inst(24))
    val b14_0 = Mux(inst(31, 28) === 0.U, Cat(inst(19,15), inst(24,20), inst(14,12)).asSInt, 
                                                       Cat(inst(24,20), inst(14,12)).asSInt) << 2
    val b31_15 = Fill(17, sign).asSInt

    Cat(b31_15, b14_0).asSInt
  }
}

object NpInstrImmGen {
  def apply(signed:Boolean, inst: UInt) = {
    val sign = if(signed) Mux(inst(31, 28) === 0.U, inst(19), inst(24)) else 0.U(1.W)
    val b9_0 = Cat( inst(24,20), inst(14,12) ) << 2
    val b14_10 = Mux(inst(31, 28) === 0.U, inst(19,15).asSInt, sign.asSInt).asUInt
    val b31_15 = Fill(17, sign).asUInt

    val imm = Mux(inst(31, 28).asUInt === 2.U, Cat(0.U(22.W), inst(24, 15)).asSInt, Cat(b31_15, b14_10, b9_0).asSInt)
    chisel3.dontTouch(imm)
    imm
  }
}

import NpInstructions._
class NpDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                        // legal jal       sel_alu2        sel_imm                                 mclk
                        //   |   | jalr    |     sel_alu1  |      alu_dw                           | wind
                        //   |   | | rxs2  |       |       |      |      alu_fn  mem      wxd      | | acc
                        //   | br| | | rxs1|       |       |      |      |       | mem_cmd|  csr   | | | 
  //JAL->               List(Y,N,Y,N,N,N,  A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,FN_ADD, N,M_X,   Y,CSR.N, N,N,N),
  //JALR->              List(Y,N,N,Y,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X,   Y,CSR.N, N,N,N),
    LPKTWJAL  ->        List(Y,N,Y,N,N,N,  A2_SIZE, A1_PC, IMM_UJ,DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,Y),
    LRESWJAL  ->        List(Y,N,Y,N,N,N,  A2_SIZE, A1_PC, IMM_UJ,DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,Y),
    LRDJAL    ->        List(Y,N,Y,N,N,N,  A2_SIZE, A1_PC, IMM_UJ,DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,Y),
    LPKTWJALR ->        List(Y,N,N,Y,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,Y),
    LRESWJALR ->        List(Y,N,N,Y,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,Y),
    LRDJALR   ->        List(Y,N,N,Y,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,Y),

  //LB->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,CSR.N, N,N,N),
  //LH->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,CSR.N, N,N,N),
  //LW->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,CSR.N, N,N,N),
    SWAPPKTB  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPPKTH  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPPKTW  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPPKTD  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPPKTUB ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPPKTUH ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPPKTUW ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPRESB  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPRESH  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPRESW  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPRESD  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPRESUB ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPRESUH ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N),
    SWAPRESUW ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,CSR.N, N,Y,N))
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


class ThreadUop extends Bundle with NpusParams {
  val valid = Bool() // valid after decode
  val ctrl = new InstrCtrlSigs
  val tid = UInt(log2Ceil(numThread).W)
  val pc = UInt(addrWidth.W) // valid after decode
  val inst = Bits(instWidth.W) // valid after decode
  val rd_valid = Bool() // valid after alu/lsu
  val rs1_valid = Bool() // true if bypassed
  val rs2_valid = Bool() // true if bypassed
  val rd = UInt((log2Ceil(numThread)+5).W) // valid after decode, bit40 for register index, others for tid
  val rs1 = UInt((log2Ceil(numThread)+5).W) // valid after decode, bit40 for register index, others for tid
  val rs2 = UInt((log2Ceil(numThread)+5).W) // valid after decode, bit4~0 for register index, others for tid
  val rd_data = Bits(dataWidth.W) // valid after alu/lsu/mul/div/fpu
  val rs1_data = Bits(dataWidth.W) // valid at the end of ISSUE
  val rs2_data = Bits(dataWidth.W) // valid at the end of ISSUE
  val make_ready = Bool() // mark mem/acc responed
}






