/*
 * 
 * A UART is a serial port, also called an RS232 interface.
 * 
 */

package npus

import chisel3._
import chisel3.util._

import freechips.rocketchip.util._
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.diplomacy.{ AddressSet, LazyModule, LazyModuleImp, RegionType, LazyRawModuleImp }
import freechips.rocketchip.tile._
import chisel3.experimental.chiselName
import java.nio.ByteBuffer
import java.nio.file.{Files,Paths}


trait ThreadsParams {
  val numThread: Int = 11
  val inslen: Int = 32
  val iramddrlen: Int = 32
  val dramddrlen: Int = 32
  val xLen: Int = 64
  val isSyncReadMem: Boolean = true
}



class IRAMBundle(datalen: Int, addrlen: Int) extends Bundle with ThreadsParams 
{
  val read = Valid( new Bundle {
              val addr = UInt(addrlen.W)
              val tid = UInt(log2Up(numThread).W) })

  val resp = Flipped( Valid( new Bundle {
              val data = UInt(datalen.W)
              val addr = UInt(addrlen.W)
              val tid = UInt(log2Up(numThread).W) }))

  val write = Valid( new Bundle {
              val addr = UInt(addrlen.W)
              val data = UInt(datalen.W)
              val tid = UInt(log2Up(numThread).W) })

  override def cloneType: this.type = (new IRAMBundle(datalen, addrlen)).asInstanceOf[this.type]
}

class IRAM(depth: Int, numRead: Int, datalen: Int) extends Module 
{
  val io = IO(Flipped(new IRAMBundle(datalen = datalen, addrlen = log2Up(depth))))

  val read_valid = RegNext(io.read.valid)
  val read_addr = RegNext(io.read.bits.tid)
  val read_tid = RegNext(io.read.bits.tid)
  val sram = SyncReadMem(depth, UInt(datalen.W))//; chisel3.dontTouch(sram)

  io.resp.valid := read_valid
  io.resp.bits.addr := read_addr
  io.resp.bits.tid := read_tid
  io.resp.bits.data := sram.read(io.read.bits.addr)

  when(io.write.valid) { sram.write(io.write.bits.addr, io.write.bits.data) }
}


class DRAMBundle(datalen: Int, addrlen: Int) extends Bundle with ThreadsParams 
{
  val req = Valid( new Bundle {
                    val cmd = UInt(M_SZ.W) /* dmem_req.ctrl.mem_cmd */
                    val size = UInt(2.W) /* dmem_req.inst_32(13,12) */
                    val signed = UInt(1.W) /* !dmem_req.inst_32(14) */
                    val data = UInt(datalen.W)
                    val addr = UInt(32.W)
                    val tid = UInt(log2Up(numThread).W) })

  val resp = Flipped( Valid( new Bundle {
                    val data = UInt(datalen.W)
                    val addr = UInt(32.W)
                    val tid = UInt(log2Up(numThread).W) }))

  override def cloneType: this.type = (new DRAMBundle(datalen, addrlen)).asInstanceOf[this.type]
}


class SyncReadMemSim(depth: Int, datalen: Int) extends Module
{
  val io = IO(new Bundle {
                    val addr = Input(UInt(log2Up(depth).W))
                    val wen = Input(Bool())
                    val wdata = Input(UInt(datalen.W))
                    val rdata = Output(UInt(datalen.W))
                    })

  val array = Mem(depth, UInt(datalen.W))
  val raddr = Reg(UInt(log2Up(depth).W))

  raddr := io.addr
  io.rdata := array(raddr)

  when(io.wen) { array(io.addr) := io.wdata }
}


class BOOTROM(depth: Int, datalen: Int) extends Module
{
  val io = IO(new Bundle{
                    val thread = Flipped(new IRAMBundle(datalen = datalen, addrlen = log2Up(depth)))
                    val dmem = Flipped(new IRAMBundle(datalen = 64, addrlen = 32))
                    })

  private lazy val img_contents = {
            val romdata = Files.readAllBytes(Paths.get("./bootrom/bootrom.img"))
            val rom = ByteBuffer.wrap(romdata)
            rom.array()}

  val contents = new FileData(img_contents).Bytes
  val wrapSize = 1 << log2Ceil(contents.size)
  require (wrapSize <= depth*datalen)
  val words = (contents ++ Seq.fill(wrapSize-contents.size)(0.toByte)).grouped(4).toSeq
  val bigs = words.map(_.foldRight(BigInt(0)){ case (x,y) => (x.toInt & 0xff) | y << 8})
  val rom = Chisel.Vec(bigs.map(x => x.U((8*4).W)))
  val code_read_valid = RegNext(io.thread.read.valid)
  val code_read_addr = RegNext(io.thread.read.bits.addr)
  val code_read_tid = RegNext(io.thread.read.bits.tid)
  val code_resp_data = RegNext(rom(io.thread.read.bits.addr >> 2))

  io.thread.resp.valid := code_read_valid
  io.thread.resp.bits.addr := code_read_addr
  io.thread.resp.bits.tid := code_read_tid
  io.thread.resp.bits.data := code_resp_data

  val dmem_read_valid = RegNext(io.dmem.read.valid)
  val dmem_read_addr = RegNext(io.dmem.read.bits.addr)
  val dmem_read_tid = RegNext(io.dmem.read.bits.tid)
  val dmem_read_addr_l = WireInit(Cat(io.dmem.read.bits.addr(log2Up(depth)-1, 2)));chisel3.dontTouch(dmem_read_addr_l)
  val dmem_read_addr_h = WireInit(Cat(io.dmem.read.bits.addr(log2Up(depth)-1, 3), 1.U(1.W)));chisel3.dontTouch(dmem_read_addr_h)
  val dmem_resp_data_l = RegNext(rom(dmem_read_addr_l));chisel3.dontTouch(dmem_resp_data_l)
  val dmem_resp_data_h = RegNext(rom(dmem_read_addr_h));chisel3.dontTouch(dmem_resp_data_h)

  io.dmem.resp.valid := dmem_read_valid
  io.dmem.resp.bits.addr := dmem_read_addr
  io.dmem.resp.bits.tid := dmem_read_tid
  io.dmem.resp.bits.data := Cat(dmem_resp_data_h, dmem_resp_data_l)
}


class FrontendIO(numThread: Int, inslen: Int, addrlen: Int) extends Bundle {
  val iram = new IRAMBundle(datalen = inslen, addrlen = addrlen)
}


class ThreadUop extends Bundle with ThreadsParams {
  val ready = Bool()
  val valid = Bool() // valid after decode
  val tid = UInt(log2Up(numThread).W)
  val ctrl = new ThreadIntCtrlSigs
  val pc = UInt(32.W) // valid after decode
  val rvc = Bool() // valid after decode
  val inst_raw = Bits(32.W) // valid after decode
  val inst_32 = Bits(32.W) // valid after decode
  val rd_valid = Bool() // valid after alu/lsu/mul/div/fpu
  val rd = UInt(5.W) // valid after decode, bit40 for register index
  val rs1 = UInt(5.W) // valid after decode, bit40 for register index
  val rs2 = UInt(5.W) // valid after decode, bit4~0 for register index
  val rd_data = Bits(64.W) // valid after alu/lsu/mul/div/fpu
  val rs1_data = Bits(64.W) // valid at the end of ISSUE
  val rs2_data = Bits(64.W) // valid at the end of ISSUE
  val dmem_addr = Bits(64.W) // valid at the end of ISSUE
  val resped = Bool() // mark mem/custom responed
}


class ThreadRegFile(n: Int, w: Int) extends Module
{
  val io = IO(new Bundle {
                    val rs1 = Input(UInt(log2Up(n).W))
                    val rs2 = Input(UInt(log2Up(n).W))
                    val rd = Input(UInt(log2Up(n).W))
                    val rs1_data = Output(UInt(w.W))
                    val rs2_data = Output(UInt(w.W))
                    val rd_write = Input(Bool())
                    val rd_data = Input(UInt(w.W))
                    })
  dontTouch(io)

  val rf = Mem(n, UInt(w.W))
  when(reset.asBool)
  {
    for(i <- 0 until n)
    rf(i) := 0.U(w.W)
  }

  io.rs1_data := rf(io.rs1)
  io.rs2_data := rf(io.rs2)

  when(io.rd_write && io.rd =/= 0.U) { rf(io.rd) := io.rd_data }
}


class ThreadScheduler extends Module with ThreadsParams
{
  val io = IO(new Bundle() {
                    val idles = Input(UInt(numThread.W))
                    val issues = Output(UInt(numThread.W))
                    })

  val issues_cnt = PopCount(io.issues)
  //if(numThread > 1) { assert((io.idles =/= (-1.S(numThread.W)).asUInt)) } // can not be all idle
  assert(issues_cnt === 1.U || issues_cnt === 0.U) // one thread per clock, otherwise error

  val rrcnt = RegInit(0.U((log2Up(2*numThread)).W))
  val rrcnt_nxt = Wire(UInt((log2Up(2*numThread)).W))
  rrcnt := rrcnt_nxt

  val expend_idles = Wire(UInt((2*numThread).W));chisel3.dontTouch(expend_idles)
  expend_idles := Cat(io.idles.asUInt, io.idles.asUInt)

  val shifted_idles = PriorityEncoderOH(expend_idles >> rrcnt) << rrcnt; chisel3.dontTouch(shifted_idles)

  val big_rrcnt = OHToUInt(shifted_idles)
  rrcnt_nxt := Mux(big_rrcnt >= numThread.U, big_rrcnt - numThread.U, big_rrcnt)

  when(reset.asBool)
  { io.issues := 0.U }
  .otherwise
  { io.issues := (io.idles.orR).asUInt << rrcnt_nxt }

}


class CSRFile extends Module with ThreadsParams
{
  val io = IO(new Bundle() {
                  val tid = Input(UInt(log2Up(numThread).W))
                  val rw = new Bundle {
                  val addr = Input(UInt(12.W/*CSR.ADDRSZ*/))
                  val cmd = Input(UInt(3.W/*CSR.SZ*/))
                  val wdata = Input(UInt(xLen.W))
                  val rdata = Output(UInt(xLen.W))
                  }
            })

  io.rw.rdata := (-1.S(xLen.W)).asUInt

  when(io.rw.cmd.isOneOf(CSR.S, CSR.C, CSR.W) && io.rw.addr === CSRs.mhartid.U(12.W))
  { io.rw.rdata := io.tid }
}


class CustomBundle extends Bundle with ThreadsParams 
{
  val req = Valid( new Bundle {
                        val rd = UInt(5.W)
                        val rs1 = UInt(5.W)
                        val rs2 = UInt(5.W)
                        val rs3 = UInt(5.W)
                        val rs1_data = UInt(xLen.W)
                        val rs2_data = UInt(xLen.W)
                        val rs3_data = UInt(xLen.W)
                        val tid = UInt(log2Up(numThread).W)
                        val inst = UInt(32.W)})
  val resp = Flipped( Valid( new Bundle {
                                  val rd_valid = Bool()
                                  val rd = UInt(5.W)
                                  val rd_data = UInt(xLen.W)
                                  val tid = UInt(log2Up(numThread).W) }))
  override def cloneType: this.type = (new CustomBundle).asInstanceOf[this.type]
}


class Threads() extends Module with ThreadsParams
{
  val io = IO(new Bundle() {
                val front = new FrontendIO(numThread, inslen, iramddrlen)
                val dmem = new DRAMBundle(datalen = xLen, addrlen = dramddrlen)
                val custom = new CustomBundle
              })

  io.front.iram.write.valid := false.B
  io.front.iram.write.bits := DontCare

  val scheduler = Module(new ThreadScheduler)

  val decode_table = { Seq(new CUSTOMDecode) ++: Seq(new I64Decode) ++: Seq(new IDecode) } flatMap(_.table)

  val id_rs1 = Wire(UInt(5.W));dontTouch(id_rs1);id_rs1 := 0.U
  val id_rs2 = Wire(UInt(5.W));dontTouch(id_rs2);id_rs2 := 0.U
  val id_rs1_data = Wire(UInt(xLen.W));dontTouch(id_rs1_data);id_rs1_data := 0.U
  val id_rs2_data = Wire(UInt(xLen.W));dontTouch(id_rs2_data);id_rs2_data := 0.U
  val id_uop = RegInit(0.U.asTypeOf(new ThreadUop));dontTouch(id_uop)
  val ex_uop = RegInit(0.U.asTypeOf(new ThreadUop));dontTouch(ex_uop)
  val wb_uops = RegInit(0.U.asTypeOf(Vec(numThread, new ThreadUop))); chisel3.dontTouch(wb_uops) // for simplity, i use multi wb uop, this need to do
  val regfile = (0 until numThread).map { i => Module(new ThreadRegFile(n = 32, w = xLen)) }
  val csr = Module(new CSRFile)
  val idles = Wire(Vec(numThread, Bool()))

  for(i <- 0 until numThread)
  {
    idles(i) := !((io.front.iram.resp.valid && (io.front.iram.resp.bits.tid === i.U)) ||
                  (id_uop.valid && (id_uop.tid === i.U)) ||
                  (ex_uop.valid && (ex_uop.tid === i.U)) ||
                  (wb_uops(i).valid ))
  }

  scheduler.io.idles := idles.asUInt

  id_uop.valid := io.front.iram.resp.valid
  id_uop.tid := io.front.iram.resp.bits.tid
  id_uop.pc := io.front.iram.resp.bits.addr
  id_uop.rvc := false.B
  id_uop.inst_32 := io.front.iram.resp.bits.data
  id_uop.rd_valid := false.B

  /****************** decode stage begin ********************/
  val id_ctrl = Wire(new ThreadIntCtrlSigs()).decode(id_uop.inst_32, decode_table); chisel3.dontTouch(id_ctrl)
  id_rs1 := Mux(id_ctrl.rxs1, id_uop.inst_32(19,15), 0.U)
  id_rs2 := Mux(id_ctrl.rxs2, id_uop.inst_32(24,20), 0.U)
  for(i <- 0 until numThread)
  {
    regfile(i).io.rs1 := 0.U //default setting
    regfile(i).io.rs2 := 0.U //default setting
    when(id_uop.valid && (id_uop.tid === i.U(log2Up(numThread).W)) )
    {
      regfile(i).io.rs1 := id_rs1
      regfile(i).io.rs2 := id_rs2
      id_rs1_data := regfile(i).io.rs1_data
      id_rs2_data := regfile(i).io.rs2_data
    }
  }
  /****************** decode stage end ********************/

  ex_uop := id_uop
  ex_uop.ctrl := id_ctrl
  ex_uop.rd := Mux(id_ctrl.wxd, id_uop.inst_32(11,7), 0.U)
  ex_uop.rs1 := id_rs1
  ex_uop.rs2 := id_rs2
  ex_uop.rs1_data := id_rs1_data
  ex_uop.rs2_data := id_rs2_data

  /****************** ex stage begin ********************/
  val alu = Module(new ThreadALU)
  alu.io.dw := ex_uop.ctrl.alu_dw
  alu.io.fn := ex_uop.ctrl.alu_fn
  alu.io.in2 := MuxLookup(ex_uop.ctrl.sel_alu2, 0.S,
                              Seq(  A2_RS2 -> ex_uop.rs2_data.asSInt,
                                    A2_IMM -> ImmGen(ex_uop.ctrl.sel_imm, ex_uop.inst_32),
                                    A2_SIZE -> Mux(ex_uop.rvc, 2.S, 4.S))).asUInt

  alu.io.in1 := MuxLookup(ex_uop.ctrl.sel_alu1, 0.S,
                              Seq(  A1_RS1 -> ex_uop.rs1_data.asSInt,
                                    A1_PC -> ex_uop.pc.asSInt)).asUInt
  /* handle imem request */
  val nxt_target = Mux(ex_uop.ctrl.jalr, alu.io.out/*encodeVirtualAddress(alu.io.out, alu.io.out)*/,
                          (ex_uop.pc.asSInt + Mux(ex_uop.ctrl.branch && alu.io.cmp_out, ImmGen(IMM_SB, ex_uop.inst_32),
                                                    Mux(ex_uop.ctrl.jal, ImmGen(IMM_UJ, ex_uop.inst_32),
                                                          Mux(ex_uop.rvc, 2.S, 4.S)))).asUInt); dontTouch(nxt_target);
  val nxt_pc = RegInit(0.U.asTypeOf(Vec(numThread, UInt(iramddrlen.W)))); chisel3.dontTouch(nxt_pc)
  for(i <- 0 until numThread)
  { 
    when(ex_uop.valid && (ex_uop.tid === i.U) ) 
    { nxt_pc(i) := Mux(ex_uop.valid, nxt_target, nxt_pc(i)) } 
  }

  io.front.iram.read.valid := scheduler.io.issues.orR //default setting
  io.front.iram.read.bits.addr := PriorityMux(scheduler.io.issues, nxt_pc)
  io.front.iram.read.bits.tid := OHToUInt(scheduler.io.issues)

  io.dmem.req.valid := ex_uop.valid && (ex_uop.ctrl.mem && ex_uop.ctrl.mem_cmd.isOneOf(M_XRD, M_XWR)) 
  io.dmem.req.bits.cmd := ex_uop.ctrl.mem_cmd
  io.dmem.req.bits.size := ex_uop.inst_32(13,12)
  io.dmem.req.bits.signed := !ex_uop.inst_32(14)
  io.dmem.req.bits.data := ex_uop.rs2_data
  io.dmem.req.bits.addr := alu.io.adder_out
  io.dmem.req.bits.tid := ex_uop.tid

  io.custom.req.valid := ex_uop.valid && (ex_uop.ctrl.custom)
  io.custom.req.bits.rd := ex_uop.rd
  io.custom.req.bits.rs1 := ex_uop.rs1
  io.custom.req.bits.rs2 := ex_uop.rs2
  io.custom.req.bits.rs3 := 0.U
  io.custom.req.bits.rs1_data := ex_uop.rs1_data
  io.custom.req.bits.rs2_data := ex_uop.rs2_data
  io.custom.req.bits.rs3_data := 0.U
  io.custom.req.bits.tid := ex_uop.tid
  io.custom.req.bits.inst := ex_uop.inst_32
  /****************** ex stage end ********************/

  csr.io.tid := ex_uop.tid
  csr.io.rw.addr := ex_uop.inst_32(31,20)
  csr.io.rw.cmd := ex_uop.ctrl.csr
  csr.io.rw.wdata := 0.U
  for(i <- 0 until numThread)
  {
  when(ex_uop.valid && (ex_uop.tid === i.U))
  {
    wb_uops(i) := ex_uop
    wb_uops(i).resped := false.B
    wb_uops(i).rd_valid := ex_uop.valid && ex_uop.ctrl.wxd && !ex_uop.ctrl.custom && !ex_uop.ctrl.mem
    wb_uops(i).rd_data := Mux(ex_uop.ctrl.csr.isOneOf(CSR.S, CSR.C, CSR.W), csr.io.rw.rdata, alu.io.out)
  }
  .elsewhen(wb_uops(i).valid && !wb_uops(i).ctrl.custom && !wb_uops(i).ctrl.mem)
  {
    wb_uops(i).valid := false.B
    wb_uops(i).rd_valid := false.B
  }
  when(io.dmem.resp.valid && (io.dmem.resp.bits.tid === i.U) && wb_uops(i).valid && wb_uops(i).ctrl.mem_cmd.isOneOf(M_XRD, M_XWR)) 
  { 
    wb_uops(i).resped := true.B 
    wb_uops(i).rd_valid := wb_uops(i).ctrl.mem_cmd === M_XRD //io.dmem.resp.valid 
    wb_uops(i).rd_data := io.dmem.resp.bits.data 
  } 
  .elsewhen(io.custom.resp.valid && (io.custom.resp.bits.tid === i.U) && wb_uops(i).valid && wb_uops(i).ctrl.custom) 
  { 
    wb_uops(i).resped := true.B 
    wb_uops(i).rd_valid := io.custom.resp.bits.rd_valid 
    //wb_uops(i).rd := io.custom.resp.bits.rd do we need ? by dongdeji 
    wb_uops(i).rd_data := io.custom.resp.bits.rd_data 
  } 
  .elsewhen(wb_uops(i).resped && wb_uops(i).valid && wb_uops(i).ctrl.mem_cmd.isOneOf(M_XRD, M_XWR)) 
  { 
    wb_uops(i).valid := false.B 
    wb_uops(i).rd_valid := false.B }
  }
  /****************** wb stage begin *******************/
  for(i <- 0 until numThread)
  {
    regfile(i).io.rd := wb_uops(i).rd
    regfile(i).io.rd_write := wb_uops(i).valid && wb_uops(i).rd_valid && wb_uops(i).ctrl.wxd
    regfile(i).io.rd_data := wb_uops(i).rd_data
  }
  /***************** wb stage end ********************/
}


class ResetGen(level: Int = 1) extends Module {
  val io = IO(new Bundle() {
              val out = Output(Bool())
           })
  var reset_out = WireInit(reset.asBool)
  io.out := RegNext(reset_out)
}


class ThreadSocTop() extends Module with ThreadsParams
{
  val io = IO(new Bundle { }) 
  val resetGen = Module(new ResetGen(1)) 
  resetGen.suggestName("top_reset_gen") 

  withClockAndReset(clock, resetGen.io.out) 
  { 
    val threads = Module(new Threads());chisel3.dontTouch(threads.io) 
    val bootrom = Module(new BOOTROM(depth = 8194, datalen = inslen)); chisel3.dontTouch(bootrom.io) 
    //val dmem = Module(new DRAM(depth = 8*1024*1024, datalen = 64)) ;chisel3.dontTouch(dmem.io) sram bug, need to do by dongdeji
    val dmem = Module(new DRAMSim(depth = 8*1024*1024, datalen = 64)) ;chisel3.dontTouch(dmem.io) 
    bootrom.io.thread <> threads.io.front.iram 
    bootrom.io.dmem <> dmem.io.bootrom 
    dmem.io.thread <> threads.io.dmem 
    dmem.io.custom <> threads.io.custom 
  }
}


object ThreadMain extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new ThreadSocTop(), Array("--target-dir", "generated",
                                                                        "--full-stacktrace",
                                                                        "--output-file", "ThreadSocTop.v",
                                                                        "--infer-rw", " ",
                                                                        "--gen-mem-verilog", "full"))

  //(new chisel3.stage.ChiselStage).emitVerilog(new ThreadSocTop(), Array("--help"))
}


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




class npusTop()(implicit p: Parameters) extends LazyModule {

  ElaborationArtefacts.add("graphml", graphML)
  ElaborationArtefacts.add("plusArgs", PlusArgArtefacts.serialize_cHeader)
  
  val fuzz = LazyModule(new TLFuzzer(10))
  val model = LazyModule(new TLRAMModel("Xbar"))
  val tlxbar = LazyModule(new TLXbar)
  val axi4xbar = AXI4Xbar()
  val beatBytes = 8

  tlxbar.node := TLDelayer(0.1) := model.node := fuzz.node
  (0 until 2) foreach { n =>
    val ram  = LazyModule(new TLRAM(address = AddressSet(0x0+0x400*n, 0x3ff), beatBytes = beatBytes))
    ram.node := TLFragmenter(8, 256) := TLDelayer(0.1) := tlxbar.node
  }

  /* iram */
  val iramxbars = Seq.tabulate(1) 
  { i => 
    val iramxbar = AXI4Xbar()
    val iram = LazyModule(new AXI4ROM(AddressSet(0x10000 + 0x400*i, 0x3ff), beatBytes = beatBytes))
    iram.node := iramxbar
    iramxbar
  }
  iramxbars.foreach { x => (x := AXI4Buffer(BufferParams.flow) := axi4xbar) }

  /*val clusterxbars = Seq.tabulate(1)
  { i => 
    val clusterxbar = AXI4Xbar()
    val masternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                          masters = Seq(AXI4MasterParameters(
                                                          name = s"Cluster_$i",
                                                          id   = IdRange(0, 1 << params.idBits))))))
    clusterxbar := AXI4IdIndexer(1/*fifoBits*/) := masternode
  }*/
  val masternode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
                                        masters = Seq(AXI4MasterParameters(
                                                        name = s"Cluster_test",
                                                        id   = IdRange(0, 1 << 1))))))
  axi4xbar := AXI4Deinterleaver(4096) := TLToAXI4() := tlxbar.node
  val matchslavenode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = Seq(AddressSet(0x7000000 + 0x400, 0x3ff)),
      //resources     = resources,
      regionType    = if (true) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = true,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    requestKeys = if (true) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))
  matchslavenode := axi4xbar := AXI4IdIndexer(1/*fifoBits*/) :=  masternode

  val cluster = LazyModule(new Cluster(0))
  axi4xbar := cluster.axi4xbar

  lazy val module = new LazyRawModuleImp(this) {
    val io = IO(new Bundle {
      val clock = Input(Clock())
      val reset = Input(Bool())
      val success = Output(Bool())
      val start = Input(Bool())
    })

    val matchperph = matchslavenode.makeIOs()

    chisel3.dontTouch(io)
    io.success := fuzz.module.io.finished
    withClockAndReset(io.clock, io.reset) {
      val threadsoctop = Module(new ThreadSocTop())
      chisel3.dontTouch(threadsoctop.io)
    }
  }
}

object TopMain extends App with HasRocketChipStageUtils {
  override def main(args: Array[String]): Unit = {
    implicit val p = Parameters.empty
    (new chisel3.stage.ChiselStage).execute(args, Seq(
      chisel3.stage.ChiselGeneratorAnnotation(() => {
        val soc = LazyModule(new npusTop())
        soc.module
      })
    ))

    ElaborationArtefacts.files.foreach{ case (extension, contents) =>
      writeOutputFile("./generated-src", s"npusTop.${extension}", contents())
    }
  }
}






