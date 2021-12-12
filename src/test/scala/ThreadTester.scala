/*
 * by dongdeji, dongdeji@126.com
 * 
 */

package thread

import chisel3._
import chisel3.iotesters.PeekPokeTester



class ThreadTester(dut: ThreadSocTop) extends PeekPokeTester(dut) {

  step(2000)
}

/*object TxTester extends App {
  iotesters.Driver.execute(Array("--target-dir", "generated", "--generate-vcd-output", "on"), () => new Tx(10000, 3000)) {
    c => new TxTester(c)
  }
}
*/
object ThreadTester extends App {
  /*(new chisel3.stage.ChiselStage).emitVerilog(new ThreadSocTop(), Array("--target-dir", "generated",
                                                                        "--generate-vcd-output", "on", 
                                                                        "--full-stacktrace",
                                                                        "--output-file", "ThreadSocTop.v",
                                                                        "--infer-rw", " ",
                                                                        "--repl-seq-mem", "-c:thread.ThreadSocTop:-o:ThreadSocTop.v.conf"))*/

  iotesters.Driver.execute(Array("--target-dir", "generated",
                                "--generate-vcd-output", "on", 
                                "--full-stacktrace",
                                "--output-file", "ThreadSocTop",
                                "--infer-rw", " ",
                                "--repl-seq-mem", "-c:npus.ThreadSocTop:-o:ThreadSocTop.v.conf"), 
                          () => new ThreadSocTop()) { c => new ThreadTester(c) }
}






