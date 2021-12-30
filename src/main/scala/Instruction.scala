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

object NpuCmd {
  val NP_CMD = 2
  def NP_X    = BitPat("b??");
  def NP_LWI  = 0.U;
  def NP_LKX  = 1.U;//BitPat("b01");
  def NP_SWAP = 2.U;//BitPat("b10");
  def NP_STK  = 3.U;//BitPat("b11");
}

object XdValid {
  val XD_VALID = 2
  def XD_X   = BitPat("b??");
  def XD_RR1 = BitPat("b00");
  def XD_ALU = BitPat("b01");
  def XD_MEM = BitPat("b10");
  def XD_LLL = BitPat("b11");
}
import NpuALU._
import NpuCmd._
import XdValid._

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
  val wxdv = Bits(XD_VALID.W)
  val csr = Bits(CSR.SZ.W)
  val npi = Bool() //np instruction
  val npcmd = Bits(NP_CMD.W)

  def default: List[BitPat] =
                        // legal jal       sel_alu2        sel_imm                                
                        //   |   | jalr    |     sel_alu1  |      alu_dw         mem_cmd  wxd          
                        //   | br| | rxs2  |       |       |      |      alu_fn mem|      | wxdv    csr  npi
                        //   | | | | | rxs1|       |       |      |      |       | |      | |       |     | npcmd
                        List(N,X,X,X,X,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   X,XD_X  , CSR.X,X,NP_X   )

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, br, jal, jalr, rxs2, rxs1, sel_alu2, sel_alu1, sel_imm, 
                   alu_dw, alu_fn, mem, mem_cmd, wxd, wxdv, csr, npi, npcmd)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class IDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                        // legal jal       sel_alu2        sel_imm                                
                        //   |   | jalr    |     sel_alu1  |      alu_dw         mem_cmd  wxd          
                        //   | br| | rxs2  |       |       |      |      alu_fn mem|      | wxdv    csr  npi
                        //   | | | | | rxs1|       |       |      |      |       | |      | |       |     | npcmd
    BNE->               List(Y,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE, N,M_X,   N,XD_X  , CSR.N,N,NP_X   ),
    BEQ->               List(Y,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ, N,M_X,   N,XD_X  , CSR.N,N,NP_X   ),
    BLT->               List(Y,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT, N,M_X,   N,XD_X  , CSR.N,N,NP_X   ),
    BLTU->              List(Y,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,N,M_X,   N,XD_X  , CSR.N,N,NP_X   ),
    BGE->               List(Y,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE, N,M_X,   N,XD_X  , CSR.N,N,NP_X   ),
    BGEU->              List(Y,Y,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,N,M_X,   N,XD_X  , CSR.N,N,NP_X   ),
 
    JAL->               List(Y,N,Y,N,N,N,  A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,FN_ADD, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    JALR->              List(Y,N,N,Y,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    AUIPC->             List(Y,N,N,N,N,N,  A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
 
    LB->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,XD_LLL, CSR.N,N,NP_X   ),
    LH->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,XD_LLL, CSR.N,N,NP_X   ),
    LW->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,XD_LLL, CSR.N,N,NP_X   ),
    LBU->               List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,XD_LLL, CSR.N,N,NP_X   ),
    LHU->               List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,XD_LLL, CSR.N,N,NP_X   ),
    SB->                List(Y,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD, Y,M_XWR, N,XD_X  , CSR.N,N,NP_X   ),
    SH->                List(Y,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD, Y,M_XWR, N,XD_X  , CSR.N,N,NP_X   ),
    SW->                List(Y,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD, Y,M_XWR, N,XD_X  , CSR.N,N,NP_X   ),
 
    LUI->               List(Y,N,N,N,N,N,  A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    ADDI->              List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SLTI ->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SLTIU->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    ANDI->              List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    ORI->               List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,  N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    XORI->              List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    ADD->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SUB->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SLT->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SLTU->              List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    AND->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    OR->                List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,  N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    XOR->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SLL->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,  N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SRL->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,  N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SRA->               List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    
    SCALL->             List(Y,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   N,XD_X  , CSR.I,N,NP_X   ),
    SBREAK->            List(Y,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   N,XD_X  , CSR.I,N,NP_X   ),
    MRET->              List(Y,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   N,XD_X  , CSR.I,N,NP_X   ),
    WFI->               List(Y,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   N,XD_X  , CSR.I,N,NP_X   ),
    CEASE->             List(Y,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   N,XD_X  , CSR.I,N,NP_X   ),
    CSRRW->             List(Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD, N,M_X,   Y,XD_MEM, CSR.W,N,NP_X   ),
    CSRRS->             List(Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD, N,M_X,   Y,XD_MEM, CSR.S,N,NP_X   ),
    CSRRC->             List(Y,N,N,N,N,Y,  A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD, N,M_X,   Y,XD_MEM, CSR.C,N,NP_X   ),
    CSRRWI->            List(Y,N,N,N,N,N,  A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD, N,M_X,   Y,XD_MEM, CSR.W,N,NP_X   ),
    CSRRSI->            List(Y,N,N,N,N,N,  A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD, N,M_X,   Y,XD_MEM, CSR.S,N,NP_X   ),
    CSRRCI->            List(Y,N,N,N,N,N,  A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD, N,M_X,   Y,XD_MEM, CSR.C,N,NP_X   ))
}


class I64Decode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                        // legal jal       sel_alu2        sel_imm                                
                        //   |   | jalr    |     sel_alu1  |      alu_dw         mem_cmd  wxd          
                        //   | br| | rxs2  |       |       |      |      alu_fn mem|      | wxdv    csr  npi
                        //   | | | | | rxs1|       |       |      |      |       | |      | |       |     | npcmd
    LD->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,XD_LLL, CSR.N,N,NP_X   ),
    LWU->               List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,XD_LLL, CSR.N,N,NP_X   ),
    
    SD->                List(Y,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD, Y,M_XWR, N,XD_X  , CSR.N,N,NP_X   ),
 
    SLLI->              List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,  N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SRLI->              List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,  N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SRAI->              List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA, N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
 
    ADDIW->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,  N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SLLIW->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_SL,   N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SRLIW->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_SR,   N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SRAIW->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_SRA,  N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    ADDW->              List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_ADD,  N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SUBW->              List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_SUB,  N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SLLW->              List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_SL,   N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SRLW->              List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_SR,   N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ),
    SRAW->              List(Y,N,N,N,Y,Y,  A2_RS2, A1_RS1, IMM_X, DW_32,FN_SRA,  N,M_X,   Y,XD_ALU, CSR.N,N,NP_X   ))
}



//            | b31~b28     | b27~b25   | b24~b20   | b19~b15    | b14~12     | b11~b7        | b6~b0             |
// LPKTWJAL   | funct4:0000 | func3:000 | imm[9:5]  | imm[14:10] | imm[4:2]   | rd            | coustom0: 0001011 | -- acc load to pkt wind + jal
// LKRESWJAL  | funct4:0001 | func3:100 | imm[9:5]  | src_table  | imm[4:2]   | rd            | coustom0: 0001011 | -- load key to match engin and retun to result wind + jal
// LKRDJAL    | funct4:0001 | func3:011 | imm[9:5]  | src_table  | imm[4:2]   | rd            | coustom0: 0001011 | -- load key to match engin and retun to rd + jal
// LPKTWJALR  | funct4:0001 | func3:000 | imm[9:5]  | rs1        | imm[4:2]   | rd            | coustom0: 0001011 | -- acc load to pkt wind + jalr
// LRESWJALR  | funct4:0001 | func3:001 | imm[9:5]  | rs1        | imm[4:2]   | rd            | coustom0: 0001011 | -- acc load to result wind + jalr
// LRDJALR    | funct4:0001 | func3:010 | imm[9:5]  | rs1        | imm[4:2]   | rd            | coustom0: 0001011 | -- acc load to rd + jalr
// SWAPPKTB   | funct4:0010 | func3:000 | windoffset[9:0]        | U+size:000 | rd            | coustom0: 0001011 | -- swap pkt wind byte
// SWAPPKTH   | funct4:0010 | func3:000 | windoffset[9:0]        | U+size:001 | rd            | coustom0: 0001011 | -- swap pkt wind halfword
// SWAPPKTW   | funct4:0010 | func3:000 | windoffset[9:0]        | U+size:010 | rd            | coustom0: 0001011 | -- swap pkt wind word
// SWAPPKTD   | funct4:0010 | func3:000 | windoffset[9:0]        | U+size:011 | rd            | coustom0: 0001011 | -- swap pkt wind doubleword
// SWAPPKTBU  | funct4:0010 | func3:000 | windoffset[9:0]        | U+size:100 | rd            | coustom0: 0001011 | -- swap pkt wind unsigned byte
// SWAPPKTHU  | funct4:0010 | func3:000 | windoffset[9:0]        | U+size:101 | rd            | coustom0: 0001011 | -- swap pkt wind unsigned halfword
// SWAPPKTWU  | funct4:0010 | func3:000 | windoffset[9:0]        | U+size:110 | rd            | coustom0: 0001011 | -- swap pkt wind unsigned word
// SWAPRESB   | funct4:0010 | func3:001 | windoffset[9:0]        | U+size:000 | rd            | coustom0: 0001011 | -- swap result wind byte
// SWAPRESH   | funct4:0010 | func3:001 | windoffset[9:0]        | U+size:001 | rd            | coustom0: 0001011 | -- swap result wind halfword
// SWAPRESW   | funct4:0010 | func3:001 | windoffset[9:0]        | U+size:010 | rd            | coustom0: 0001011 | -- swap result wind word
// SWAPRESD   | funct4:0010 | func3:001 | windoffset[9:0]        | U+size:011 | rd            | coustom0: 0001011 | -- swap result wind doubleword
// SWAPRESUB  | funct4:0010 | func3:001 | windoffset[9:0]        | U+size:100 | rd            | coustom0: 0001011 | -- swap result wind unsigned byte
// SWAPRESUH  | funct4:0010 | func3:001 | windoffset[9:0]        | U+size:101 | rd            | coustom0: 0001011 | -- swap result wind unsigned halfword
// SWAPRESUW  | funct4:0010 | func3:001 | windoffset[9:0]        | U+size:110 | rd            | coustom0: 0001011 | -- swap result wind unsigned word
// STOREKEYBU | funct4:0010 | func3:010 | rs2        |func5:00000| U+size:100 | reserve:00000 | coustom0: 0001011 | -- store unsigned byte key to buff
// STOREKEYHU | funct4:0010 | func3:010 | rs2        |func5:00000| U+size:101 | reserve:00000 | coustom0: 0001011 | -- store unsigned halftword key to buff
// STOREKEYWU | funct4:0010 | func3:010 | rs2        |func5:00000| U+size:110 | reserve:00000 | coustom0: 0001011 | -- store unsigned word key to buff
// STOREKEYDU | funct4:0010 | func3:010 | rs2        |func5:00000| U+size:111 | reserve:00000 | coustom0: 0001011 | -- store unsigned doubleword key to buff


/* Automatically generated by parse-opcodes */
object NpInstructions {
  def LPKTWJAL          = BitPat("b0000000??????????????????0001011")
  def LKRESWJAL         = BitPat("b0001100??????????????????0001011")
  def LKRDJAL           = BitPat("b0001011??????????????????0001011")
  def LPKTWJALR         = BitPat("b0001000??????????????????0001011")
  def LRESWJALR         = BitPat("b0001001??????????????????0001011")
  def LRDJALR           = BitPat("b0001010??????????????????0001011")
  def SWAPPKTB          = BitPat("b0010000??????????000?????0001011")
  def SWAPPKTH          = BitPat("b0010000??????????001?????0001011")
  def SWAPPKTW          = BitPat("b0010000??????????010?????0001011")
  def SWAPPKTD          = BitPat("b0010000??????????011?????0001011")
  def SWAPPKTBU         = BitPat("b0010000??????????100?????0001011")
  def SWAPPKTHU         = BitPat("b0010000??????????101?????0001011")
  def SWAPPKTWU         = BitPat("b0010000??????????110?????0001011")
  def SWAPRESB          = BitPat("b0010001??????????000?????0001011")
  def SWAPRESH          = BitPat("b0010001??????????001?????0001011")
  def SWAPRESW          = BitPat("b0010001??????????010?????0001011")
  def SWAPRESD          = BitPat("b0010001??????????011?????0001011")
  def SWAPRESUB         = BitPat("b0010001??????????100?????0001011")
  def SWAPRESUH         = BitPat("b0010001??????????101?????0001011")
  def SWAPRESUW         = BitPat("b0010001??????????110?????0001011")
  def STOREKEYBU        = BitPat("b0010010?????00000100000000001011")
  def STOREKEYHU        = BitPat("b0010010?????00000101000000001011")
  def STOREKEYWU        = BitPat("b0010010?????00000110000000001011")
  def STOREKEYDU        = BitPat("b0010010?????00000111000000001011")

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
    chisel3.dontTouch(b9_0)
    chisel3.dontTouch(b14_10)
    chisel3.dontTouch(b31_15)
    
    val imm = Mux(inst(31, 28).asUInt === 2.U, Cat(0.U(22.W), inst(24, 15)).asSInt, Cat(b31_15, b14_10, b9_0).asSInt)
    chisel3.dontTouch(imm)
    imm
  }
}

import NpInstructions._
class NpDecode extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
                        // legal jal       sel_alu2        sel_imm                                
                        //   |   | jalr    |     sel_alu1  |      alu_dw         mem_cmd  wxd          
                        //   | br| | rxs2  |       |       |      |      alu_fn mem|      | wxdv    csr  npi
                        //   | | | | | rxs1|       |       |      |      |       | |      | |       |     | npcmd
  //JAL->               List(Y,N,Y,N,N,N,  A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,FN_ADD, N,M_X,   Y,XD_LLL, CSR.N,Y,NP_LWI ),
  //JALR->              List(Y,N,N,Y,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X,   Y,XD_LLL, CSR.N,Y,NP_LWI ),
    LPKTWJAL  ->        List(Y,N,Y,N,N,N,  A2_SIZE, A1_PC, IMM_UJ,DW_XPR,FN_ADD, N,M_X  , Y,XD_LLL, CSR.N,Y,NP_LWI ),
    LKRESWJAL ->        List(Y,N,Y,N,N,Y,  A2_SIZE, A1_PC, IMM_UJ,DW_XPR,FN_ADD, N,M_X  , Y,XD_LLL, CSR.N,Y,NP_LKX ),
    LKRDJAL   ->        List(Y,N,Y,N,N,Y,  A2_SIZE, A1_PC, IMM_UJ,DW_XPR,FN_ADD, N,M_X  , Y,XD_LLL, CSR.N,Y,NP_LKX ),
    LPKTWJALR ->        List(Y,N,N,Y,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_LLL, CSR.N,Y,NP_LWI ),
    LRESWJALR ->        List(Y,N,N,Y,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_LLL, CSR.N,Y,NP_LWI ),
    LRDJALR   ->        List(Y,N,N,Y,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_LLL, CSR.N,Y,NP_LWI ),
  //LB->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,XD_RR1, CSR.N,Y,NP_SWAP),
  //LH->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,XD_RR1, CSR.N,Y,NP_SWAP),
  //LW->                List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, Y,M_XRD, Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPPKTB  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPPKTH  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPPKTW  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPPKTD  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPPKTBU ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPPKTHU ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPPKTWU ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPRESB  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPRESH  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPRESW  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPRESD  ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPRESUB ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPRESUH ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),
    SWAPRESUW ->        List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD, N,M_X  , Y,XD_RR1, CSR.N,Y,NP_SWAP),

  //WFI->               List(Y,N,N,N,N,X,  A2_X,   A1_X,   IMM_X, DW_X,  FN_X,   N,M_X,   N,XD_X  , CSR.I,N,NP_STK ),
  //SH->                List(Y,N,N,N,Y,Y,  A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD, Y,M_XWR, N,XD_X  , CSR.N,N,NP_STK ),
  //ADDIW->             List(Y,N,N,N,N,Y,  A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,  N,M_X,   Y,XD_X  , CSR.N,N,NP_STK ),
    STOREKEYBU->        List(Y,N,N,N,Y,N,  A2_RS2,   A1_X, IMM_X, DW_X,  FN_X,   N,M_X,   N,XD_X  , CSR.N,Y,NP_STK ),
    STOREKEYHU->        List(Y,N,N,N,Y,N,  A2_RS2,   A1_X, IMM_X, DW_X,  FN_X,   N,M_X,   N,XD_X  , CSR.N,Y,NP_STK ),
    STOREKEYWU->        List(Y,N,N,N,Y,N,  A2_RS2,   A1_X, IMM_X, DW_X,  FN_X,   N,M_X,   N,XD_X  , CSR.N,Y,NP_STK ),
    STOREKEYDU->        List(Y,N,N,N,Y,N,  A2_RS2,   A1_X, IMM_X, DW_X,  FN_X,   N,M_X,   N,XD_X  , CSR.N,Y,NP_STK ))
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

  override def cloneType: this.type = (new ThreadUop).asInstanceOf[this.type]
}






