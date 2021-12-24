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

class RrBypassMux extends Module with NpusParams
{
  val io = IO(new Bundle {
                  val rr_uop_R = Input(new ThreadUop)
                  val ex_uop_W = Input(new ThreadUop)
                  val wb_uop_W = Input(new ThreadUop)
                  val rs1_bypassed = Output(Bool())
                  val rs2_bypassed = Output(Bool())
                  val rs1_data = Output(UInt(dataWidth.W))
                  val rs2_data = Output(UInt(dataWidth.W))
                })
  chisel3.dontTouch(io)
  val rs1_bypass_ex  = (io.rr_uop_R.rs1 =/= 0.U) && io.ex_uop_W.valid && (io.rr_uop_R.rs1 === io.ex_uop_W.rd ) && io.ex_uop_W.rd_valid
  val rs1_bypass_wb  = (io.rr_uop_R.rs1 =/= 0.U) && io.wb_uop_W.valid && (io.rr_uop_R.rs1 === io.wb_uop_W.rd ) && io.wb_uop_W.rd_valid
  val rs2_bypass_ex  = (io.rr_uop_R.rs2 =/= 0.U) && io.ex_uop_W.valid && (io.rr_uop_R.rs2 === io.ex_uop_W.rd ) && io.ex_uop_W.rd_valid
  val rs2_bypass_wb  = (io.rr_uop_R.rs2 =/= 0.U) && io.wb_uop_W.valid && (io.rr_uop_R.rs2 === io.wb_uop_W.rd ) && io.wb_uop_W.rd_valid

  chisel3.dontTouch(rs1_bypass_ex)
  chisel3.dontTouch(rs1_bypass_wb)
  chisel3.dontTouch(rs2_bypass_ex)
  chisel3.dontTouch(rs2_bypass_wb)

  when(rs1_bypass_ex) // high priority
  { io.rs1_data := io.ex_uop_W.rd_data } 
  .elsewhen(rs1_bypass_wb) 
  { io.rs1_data := io.wb_uop_W.rd_data }
  .otherwise// low priority
  { io.rs1_data := io.rr_uop_R.rs1_data}

  io.rs1_bypassed := rs1_bypass_ex || rs1_bypass_wb

  when(rs2_bypass_ex) // high priority
  { io.rs2_data := io.ex_uop_W.rd_data } 
  .elsewhen(rs2_bypass_wb) 
  { io.rs2_data := io.wb_uop_W.rd_data }
  .otherwise// low priority
  { io.rs2_data := io.rr_uop_R.rs2_data}

  io.rs2_bypassed := rs2_bypass_ex || rs2_bypass_wb
}

class Core(ClusterId:Int, GroupId:Int, NpId: Int)(implicit p: Parameters) extends LazyModule with NpusParams 
{

  val window = LazyModule(new Window(ClusterId, GroupId, NpId))
  val regfile = LazyModule(new RegFiles(ClusterId, GroupId, NpId))

  lazy val module = new LazyModuleImp(this) 
  {
    val io = IO(new Bundle {
      val frontend = Flipped(new FrontEndBundle)
      val accinf = new AccInfBundle
    })
    chisel3.dontTouch(io)

    /****************************************************************/
    /**************** key pipe signal define begine******************/
    val rr0_uop_R = RegInit(0.U.asTypeOf(new ThreadUop));dontTouch(rr0_uop_R)
    val rr1_uop_R = RegInit(0.U.asTypeOf(new ThreadUop));dontTouch(rr1_uop_R)
    val rr2_uop_R = RegInit(0.U.asTypeOf(new ThreadUop));dontTouch(rr2_uop_R)
    val ex_uop_R = RegInit(0.U.asTypeOf(new ThreadUop));dontTouch(ex_uop_R)
    val wb_uop_R = RegInit(0.U.asTypeOf(new ThreadUop));dontTouch(wb_uop_R)
    val id_uop_W = WireInit(0.U.asTypeOf(new ThreadUop));dontTouch(id_uop_W)
    val rr0_uop_W = WireInit(0.U.asTypeOf(new ThreadUop));dontTouch(rr0_uop_W)
    val rr1_uop_W = WireInit(0.U.asTypeOf(new ThreadUop));dontTouch(rr1_uop_W)
    val rr2_uop_W = WireInit(0.U.asTypeOf(new ThreadUop));dontTouch(rr2_uop_W)
    val ex_uop_W = WireInit(0.U.asTypeOf(new ThreadUop));dontTouch(ex_uop_W)
    val wb_uop_W = WireInit(0.U.asTypeOf(new ThreadUop));dontTouch(wb_uop_W)
    // default connection
    rr0_uop_R := id_uop_W
    rr0_uop_W := rr0_uop_R
    rr1_uop_R := rr0_uop_W
    rr1_uop_W := rr1_uop_R
    rr2_uop_R := rr1_uop_W
    rr2_uop_W := rr2_uop_R
    ex_uop_R  := rr2_uop_W
    ex_uop_W  := ex_uop_R
    wb_uop_R  := ex_uop_W 
    wb_uop_W  := wb_uop_R

    /**************** key pipe signal define end ******************/
    /**************************************************************/

    /****************************************************************/
    /****************** instruction decode begin ********************/
    val decode_table = { Seq(new CUSTOMDecode) ++: Seq(new I64Decode) ++: Seq(new IDecode) } flatMap(_.table)
    val id_ctrl = Wire(new InstrCtrlSigs()).decode(io.frontend.instr.bits.instr, decode_table); chisel3.dontTouch(id_ctrl)
    id_uop_W.valid    := io.frontend.instr.valid && id_ctrl.legal
    id_uop_W.ctrl     := id_ctrl
    id_uop_W.tid      := io.frontend.instr.bits.tid
    id_uop_W.pc       := io.frontend.instr.bits.pc
    id_uop_W.instr    := io.frontend.instr.bits.instr
    id_uop_W.rd_valid := false.B
    id_uop_W.rd       := Cat(io.frontend.instr.bits.tid, Mux(id_ctrl.wxd , io.frontend.instr.bits.instr(11, 7), 0.U(5.W)) )
    id_uop_W.rs1      := Cat(io.frontend.instr.bits.tid, Mux(id_ctrl.rxs1, io.frontend.instr.bits.instr(19,15), 0.U(5.W)) )
    id_uop_W.rs2      := Cat(io.frontend.instr.bits.tid, Mux(id_ctrl.rxs2, io.frontend.instr.bits.instr(24,20), 0.U(5.W)) )
    id_uop_W.rd_data  := 0.U
    id_uop_W.rs1_data := 0.U
    id_uop_W.rs2_data := 0.U
    id_uop_W.make_ready := io.frontend.instr.bits.halt_last

    /****************** instruction decode end **********************/
    /****************************************************************/

    /**********************************************************/
    /****************** register read begin *******************/
    regfile.module.io.rs1 :=  rr0_uop_R.rs1
    regfile.module.io.rs2 :=  rr0_uop_R.rs2
    
    window.module.io.r_offset := 0.U // to do by dongdeji
    
    //register read stage 0 bypass check
    val rr0_bypass_mux = Module(new RrBypassMux)
    rr0_bypass_mux.io.rr_uop_R := rr0_uop_R
    rr0_bypass_mux.io.ex_uop_W := ex_uop_W
    rr0_bypass_mux.io.wb_uop_W := wb_uop_W
    rr0_uop_W.rs1_valid := rr0_bypass_mux.io.rs1_bypassed
    rr0_uop_W.rs1_data := rr0_bypass_mux.io.rs1_data
    rr0_uop_W.rs2_valid := rr0_bypass_mux.io.rs2_bypassed
    rr0_uop_W.rs2_data := rr0_bypass_mux.io.rs2_data

    //register read stage 1 bypass check
    val rr1_bypass_mux = Module(new RrBypassMux)
    rr1_bypass_mux.io.rr_uop_R := rr1_uop_R
    rr1_bypass_mux.io.ex_uop_W := ex_uop_W
    rr1_bypass_mux.io.wb_uop_W := wb_uop_W
    rr1_uop_W.rs1_valid := rr1_bypass_mux.io.rs1_bypassed
    rr1_uop_W.rs1_data := rr1_bypass_mux.io.rs1_data
    rr1_uop_W.rs2_valid := rr1_bypass_mux.io.rs2_bypassed
    rr1_uop_W.rs2_data := rr1_bypass_mux.io.rs2_data

    //register read stage 2 bypass check
    val rr2_bypass_mux = Module(new RrBypassMux)
    rr2_bypass_mux.io.rr_uop_R := rr2_uop_R
    rr2_bypass_mux.io.ex_uop_W := ex_uop_W
    rr2_bypass_mux.io.wb_uop_W := wb_uop_W
    rr2_uop_W.rs1_valid := rr2_bypass_mux.io.rs1_bypassed
    rr2_uop_W.rs1_data := rr2_bypass_mux.io.rs1_data
    rr2_uop_W.rs2_valid := rr2_bypass_mux.io.rs2_bypassed
    rr2_uop_W.rs2_data := rr2_bypass_mux.io.rs2_data
    /****************** register read end *********************/
    /**********************************************************/

    /***********************************************/
    /****************** ex begin *******************/
    ex_uop_W.rs1_data := Mux(ex_uop_R.rs1_valid, ex_uop_R.rs1_data, regfile.module.io.rs1_data)
    ex_uop_W.rs2_data := Mux(ex_uop_R.rs2_valid, ex_uop_R.rs2_data, regfile.module.io.rs2_data)

    val csr = Module(new CSRFile)
    csr.io.tid := ex_uop_R.tid
    csr.io.rw.addr := ex_uop_R.instr(31,20)
    csr.io.rw.cmd := ex_uop_R.ctrl.csr
    csr.io.rw.wdata := 0.U

    val alu = Module(new NpuALU);chisel3.dontTouch(alu.io)
    alu.io.dw := ex_uop_R.ctrl.alu_dw
    alu.io.fn := ex_uop_R.ctrl.alu_fn
    alu.io.in2 := MuxLookup(ex_uop_R.ctrl.sel_alu2, 0.S,
                                Seq(  A2_RS2 -> ex_uop_W.rs2_data.asSInt,
                                      A2_IMM -> ImmGen(ex_uop_R.ctrl.sel_imm, ex_uop_R.instr),
                                      A2_SIZE -> Mux(/*ex_uop_R.rvc*/false.B, 2.S, 4.S))).asUInt

    alu.io.in1 := MuxLookup(ex_uop_R.ctrl.sel_alu1, 0.S,
                                Seq(  A1_RS1 -> ex_uop_W.rs1_data.asSInt,
                                      A1_PC -> ex_uop_W.pc.asSInt)).asUInt

    ex_uop_W.rd_valid := ex_uop_W.valid && ex_uop_R.ctrl.wxd && !ex_uop_R.ctrl.mem
    ex_uop_W.rd_data := Mux(ex_uop_R.ctrl.csr.isOneOf(CSR.S, CSR.C, CSR.W), csr.io.rw.rdata, alu.io.out)
    /* handle imem request */
    val nxt_target = Mux(ex_uop_R.ctrl.jalr, alu.io.out/*encodeVirtualAddress(alu.io.out, alu.io.out)*/,
                            (ex_uop_R.pc.asSInt + Mux(ex_uop_R.ctrl.br && alu.io.cmp_out, ImmGen(IMM_SB, ex_uop_R.instr),
                                                      Mux(ex_uop_R.ctrl.jal, ImmGen(IMM_UJ, ex_uop_R.instr),
                                                            Mux(/*ex_uop_R.rvc*/false.B, 2.S, 4.S)))).asUInt); dontTouch(nxt_target);
    val redirectForMem = if(memInstrHalt) ex_uop_R.ctrl.mem else false.B
    val redirectForAcc = ex_uop_W.valid && ex_uop_R.ctrl.acc
    val redirectForBJ = ex_uop_W.valid && ((ex_uop_R.ctrl.br && alu.io.cmp_out) || ex_uop_R.ctrl.jal || ex_uop_R.ctrl.jalr)
    io.frontend.redirect.valid := ex_uop_W.valid && (redirectForBJ || redirectForAcc || redirectForMem)
    io.frontend.redirect.bits.tid := ex_uop_R.tid
    io.frontend.redirect.bits.npc := Mux(redirectForMem || redirectForAcc, ex_uop_R.pc + 4.U, nxt_target)
    ex_uop_W.make_ready := ex_uop_R.make_ready || redirectForBJ
    // erase instrs that following the redirected instr
    class TailEraseInfo extends Bundle with NpusParams {
      val valid = Bool() // valid after decode
      val tid = UInt(log2Up(numThread).W)
    }    
    val tailEraseInfo = RegInit(0.U.asTypeOf(new TailEraseInfo));chisel3.dontTouch(tailEraseInfo)
    val acceReq = false.B
    when(io.frontend.redirect.valid )
    { 
      tailEraseInfo.valid := true.B
      tailEraseInfo.tid := ex_uop_R.tid 
    }
    when(tailEraseInfo.valid && (ex_uop_R.valid && (ex_uop_R.tid === tailEraseInfo.tid)))
    { ex_uop_W := 0.U.asTypeOf(new ThreadUop) }
    when(tailEraseInfo.valid && ((ex_uop_R.valid && (ex_uop_R.tid =/= tailEraseInfo.tid)) || (!ex_uop_R.valid)))
    { tailEraseInfo.valid := false.B }

    io.accinf.uop := ex_uop_W
    io.accinf.req.valid := ex_uop_W.valid && (ex_uop_W.ctrl.mem && ex_uop_W.ctrl.mem_cmd.isOneOf(M_XRD, M_XWR)) 
    io.accinf.req.bits.cmd := ex_uop_W.ctrl.mem_cmd
    io.accinf.req.bits.size := ex_uop_W.instr(13,12)
    io.accinf.req.bits.signed := !ex_uop_W.instr(14)
    io.accinf.req.bits.data := ex_uop_W.rs2_data
    io.accinf.req.bits.addr := alu.io.adder_out
    io.accinf.req.bits.tid := ex_uop_W.tid
    /****************** ex end *********************/
    /***********************************************/

    /*******************************************************/
    /****************** write back begin *******************/
    regfile.module.io.rd_write := wb_uop_W.valid & wb_uop_W.ctrl.legal & wb_uop_W.rd_valid
    regfile.module.io.rd_data  := wb_uop_W.rd_data
    regfile.module.io.rd       := wb_uop_W.rd

    val wb_make_ready = wb_uop_W.valid && wb_uop_W.ctrl.legal && wb_uop_W.make_ready
    io.frontend.readys.valid := wb_make_ready | io.accinf.readys.valid
    io.frontend.readys.bits.thread :=  (Fill(numThread, io.accinf.readys.valid) & io.accinf.readys.bits.thread) | 
                                       (Fill(numThread, wb_make_ready) & UIntToOH(wb_uop_W.tid))

    /****************** write back end *********************/
    /*******************************************************/

  }
}







