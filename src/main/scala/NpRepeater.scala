// See LICENSE.SiFive for license details.

package npus

import chisel3._
import chisel3.util.{Irrevocable, IrrevocableIO, Decoupled, DecoupledIO}

// A Repeater passes its input to its output, unless repeat is asserted.
// When repeat is asserted, the Repeater copies the input and repeats it next cycle.
class NpRepeater[T <: Data](gen: T) extends Module
{
  val io = IO( new Bundle {
    val repeat = Input(Bool())
    val full = Output(Bool())
    val enq = Flipped(Irrevocable(gen.cloneType))
    val deq = Irrevocable(gen.cloneType)
  } )

  val full = RegInit(false.B)
  val saved = Reg(gen.cloneType)

  // When !full, a repeater is pass-through
  io.deq.valid := io.enq.valid || full
  io.enq.ready := io.deq.ready && !full
  io.deq.bits := Mux(full, saved, io.enq.bits)
  io.full := full

  when (io.enq.fire() &&  io.repeat) { full := true.B; saved := io.enq.bits }
  when (io.deq.fire() && !io.repeat) { full := false.B }
}

object NpRepeater
{
  def apply[T <: Data](enq: IrrevocableIO[T], repeat: Bool): IrrevocableIO[T] = {
    val repeater = Module(new NpRepeater(chiselTypeOf(enq.bits)))
    repeater.io.repeat := repeat
    repeater.io.enq <> enq
    repeater.io.deq
  }
}
