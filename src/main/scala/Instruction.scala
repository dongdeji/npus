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
  val wind = Bool()
  val acc = Bool()
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

  def default: List[BitPat] =
                        // legal     jal       sel_alu2        sel_imm                             
                        //   |  wind  | jalr    |     sel_alu1  |      alu_dw                       
                        //   |  | acc | | rxs2  |       |       |      |      alu_fn    mem      wxd
                        //   |  | | br| | | rxs1|       |       |      |      |         | mem_cmd|  csr 
                        List(N, X,X,X,X,X,X,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,   X,CSR.X)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, wind , acc, br, jal, jalr, rxs2, rxs1, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd, wxd, csr)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class IDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                        // legal     jal       sel_alu2        sel_imm                             
                        //   |  wind  | jalr    |     sel_alu1  |      alu_dw                       
                        //   |  | acc | | rxs2  |       |       |      |      alu_fn    mem      wxd
                        //   |  | | br| | | rxs1|       |       |      |      |         | mem_cmd|  csr 
    BNE->               List(Y, N,N,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE,   N,M_X,   N,CSR.N),
    BEQ->               List(Y, N,N,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ,   N,M_X,   N,CSR.N),
    BLT->               List(Y, N,N,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT,   N,M_X,   N,CSR.N),
    BLTU->              List(Y, N,N,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,  N,M_X,   N,CSR.N),
    BGE->               List(Y, N,N,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE,   N,M_X,   N,CSR.N),
    BGEU->              List(Y, N,N,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,  N,M_X,   N,CSR.N),
 
    JAL->               List(Y, N,N,N,Y,N,N,N,  A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    JALR->              List(Y, N,N,N,N,Y,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    AUIPC->             List(Y, N,N,N,N,N,N,N,  A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
 
    LB->                List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    LH->                List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    LW->                List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    LBU->               List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    LHU->               List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    SB->                List(Y, N,N,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR, N,CSR.N),
    SH->                List(Y, N,N,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR, N,CSR.N),
    SW->                List(Y, N,N,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR, N,CSR.N),
 
    LUI->               List(Y, N,N,N,N,N,N,N,  A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    ADDI->              List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    SLTI ->             List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT,   N,M_X,   Y,CSR.N),
    SLTIU->             List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,  N,M_X,   Y,CSR.N),
    ANDI->              List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND,   N,M_X,   Y,CSR.N),
    ORI->               List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,    N,M_X,   Y,CSR.N),
    XORI->              List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR,   N,M_X,   Y,CSR.N),
    ADD->               List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    SUB->               List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB,   N,M_X,   Y,CSR.N),
    SLT->               List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT,   N,M_X,   Y,CSR.N),
    SLTU->              List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,  N,M_X,   Y,CSR.N),
    AND->               List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND,   N,M_X,   Y,CSR.N),
    OR->                List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,    N,M_X,   Y,CSR.N),
    XOR->               List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR,   N,M_X,   Y,CSR.N),
    SLL->               List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,    N,M_X,   Y,CSR.N),
    SRL->               List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,    N,M_X,   Y,CSR.N),
    SRA->               List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA,   N,M_X,   Y,CSR.N),

    //FENCE->             List(Y, Y,N,N,N,N,N,N,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,   N,CSR.N),

    SCALL->             List(Y, N,N,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,   N,CSR.I),
    SBREAK->            List(Y, N,N,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,   N,CSR.I),
    MRET->              List(Y, N,N,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,   N,CSR.I),
    WFI->               List(Y, N,N,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,   N,CSR.I),
    CEASE->             List(Y, N,N,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,   N,CSR.I),
    CSRRW->             List(Y, N,N,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.W),
    CSRRS->             List(Y, N,N,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.S),
    CSRRC->             List(Y, N,N,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.C),
    CSRRWI->            List(Y, N,N,N,N,N,N,N,  A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.W),
    CSRRSI->            List(Y, N,N,N,N,N,N,N,  A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.S),
    CSRRCI->            List(Y, N,N,N,N,N,N,N,  A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.C))
}


class I64Decode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                        // legal     jal       sel_alu2        sel_imm                             
                        //   |  wind  | jalr    |     sel_alu1  |      alu_dw                       
                        //   |  | acc | | rxs2  |       |       |      |      alu_fn    mem      wxd
                        //   |  | | br| | | rxs1|       |       |      |      |         | mem_cmd|  csr 
    LD->                List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    LWU->               List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    SD->                List(Y, N,N,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR, N,CSR.N),
 
    SLLI->              List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,   Y,CSR.N),
    SRLI->              List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,   Y,CSR.N),
    SRAI->              List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,   Y,CSR.N),
 
    ADDIW->             List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,    N,M_X,   Y,CSR.N),
    SLLIW->             List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_SL,     N,M_X,   Y,CSR.N),
    SRLIW->             List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_SR,     N,M_X,   Y,CSR.N),
    SRAIW->             List(Y, N,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_SRA,    N,M_X,   Y,CSR.N),
    ADDW->              List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_ADD,    N,M_X,   Y,CSR.N),
    SUBW->              List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_SUB,    N,M_X,   Y,CSR.N),
    SLLW->              List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_SL,     N,M_X,   Y,CSR.N),
    SRLW->              List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_SR,     N,M_X,   Y,CSR.N),
    SRAW->              List(Y, N,N,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_SRA,    N,M_X,   Y,CSR.N))
}


class CUSTOMDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                        // legal     jal       sel_alu2        sel_imm                             
                        //   |  wind  | jalr    |     sel_alu1  |      alu_dw                       
                        //   |  | acc | | rxs2  |       |       |      |      alu_fn    mem      wxd
                        //   |  | | br| | | rxs1|       |       |      |      |         | mem_cmd|  csr 
    CUSTOM0->           List(Y, N,Y,N,N,N,N,N,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   N,CSR.N),
    CUSTOM0_RS1->       List(Y, N,Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   N,CSR.N),
    CUSTOM0_RS1_RS2->   List(Y, N,Y,N,N,N,Y,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   N,CSR.N),
    CUSTOM0_RD->        List(Y, N,Y,N,N,N,N,N,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    CUSTOM0_RD_RS1->    List(Y, N,Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    CUSTOM0_RD_RS1_RS2->List(Y, N,Y,N,N,N,Y,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    CUSTOM1->           List(Y, N,Y,N,N,N,N,N,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   N,CSR.N),
    CUSTOM1_RS1->       List(Y, N,Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   N,CSR.N),
    CUSTOM1_RS1_RS2->   List(Y, N,Y,N,N,N,Y,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   N,CSR.N),
    CUSTOM1_RD->        List(Y, N,Y,N,N,N,N,N,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    CUSTOM1_RD_RS1->    List(Y, N,Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    CUSTOM1_RD_RS1_RS2->List(Y, N,Y,N,N,N,Y,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    CUSTOM2->           List(Y, N,Y,N,N,N,N,N,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   N,CSR.N),
    CUSTOM2_RS1->       List(Y, N,Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   N,CSR.N),
    CUSTOM2_RS1_RS2->   List(Y, N,Y,N,N,N,Y,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   N,CSR.N),
    CUSTOM2_RD->        List(Y, N,Y,N,N,N,N,N,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    CUSTOM2_RD_RS1->    List(Y, N,Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    CUSTOM2_RD_RS1_RS2->List(Y, N,Y,N,N,N,Y,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    CUSTOM3->           List(Y, N,Y,N,N,N,N,N,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   N,CSR.N),
    CUSTOM3_RS1->       List(Y, N,Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   N,CSR.N),
    CUSTOM3_RS1_RS2->   List(Y, N,Y,N,N,N,Y,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   N,CSR.N),
    CUSTOM3_RD->        List(Y, N,Y,N,N,N,N,N,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    CUSTOM3_RD_RS1->    List(Y, N,Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N),
    CUSTOM3_RD_RS1_RS2->List(Y, N,Y,N,N,N,Y,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,   Y,CSR.N))
}

// | funct4:0000 | func3:000 | imm[9:5] | Imm[14:10] | imm[4:2] | rd | coustom0: 0001011 |  -- acc load to pkt wind + jal
// | funct4:0000 | func3:001 | imm[9:5] | Imm[14:10] | imm[4:2] | rd | coustom0: 0001011 |  -- acc load to result wind + jal
// | funct4:0000 | func3:002 | imm[9:5] | Imm[14:10] | imm[4:2] | rd | coustom0: 0001011 |  -- acc load to rd + jal
// | funct4:0001 | func3:000 | imm[9:5] | rs1        | imm[4:2] | rd | coustom0: 0001011 |  -- acc load to pkt wind + jalr
// | funct4:0001 | func3:001 | imm[9:5] | rs1        | imm[4:2] | rd | coustom0: 0001011 |  -- acc load to result wind + jalr
// | funct4:0001 | func3:002 | imm[9:5] | rs1        | imm[4:2] | rd | coustom0: 0001011 |  -- acc load to rd + jalr
// | funct4:0010 | func3:000 | windoffset[9:0]       | U+size   | rd | coustom0: 0001011 |  -- pkt wind swap
// | funct4:0010 | func3:001 | windoffset[9:0]       | U+size   | rd | coustom0: 0001011 |  -- result wind swap

/* Automatically generated by parse-opcodes */
object NpInstructions {
  def NPSWAPB            = BitPat("b?????????????????000?????0001011")
  def NPSWAPH            = BitPat("b?????????????????001?????0001011")
  def NPSWAPW            = BitPat("b?????????????????010?????0001011")
  def NPSWAPD            = BitPat("b?????????????????011?????0001011")
  def NPSWAPBU           = BitPat("b?????????????????100?????0001011")
  def NPSWAPHU           = BitPat("b?????????????????101?????0001011")
  def NPSWAPWU           = BitPat("b?????????????????110?????0001011")

  def NPRCV              = BitPat("b0000000??????????111?????0001011")  
  def NPSEND             = BitPat("b0000001??????????111?????0001011")
}

import NpInstructions._

class NpDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                        // legal     jal       sel_alu2        sel_imm                             
                        //   |  wind  | jalr    |     sel_alu1  |      alu_dw                       
                        //   |  | acc | | rxs2  |       |       |      |      alu_fn    mem      wxd
                        //   |  | | br| | | rxs1|       |       |      |      |         | mem_cmd|  csr 
    NPSWAPB ->          List(Y, Y,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    NPSWAPH ->          List(Y, Y,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    NPSWAPW ->          List(Y, Y,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    NPSWAPD ->          List(Y, Y,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    NPSWAPBU->          List(Y, Y,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    NPSWAPHU->          List(Y, Y,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N),
    NPSWAPWU->          List(Y, Y,N,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD, Y,CSR.N))
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
  val instr = Bits(instrWidth.W) // valid after decode
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






