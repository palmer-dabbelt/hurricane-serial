import Chisel._

package Serial {
  // A single 8b/10b encoder/decoder, attached to the phy (for actual
  // transmission) and to the rest of the chip (via the "controller"
  // IO bundle).
  class Serial8b10bController(channel_count: Int)
  extends Module {
    class IO extends Bundle {
      val phy = new PhyIO(encoded_word_bits = 10,
                          channel_count = channel_count)
      val ctl = new ControllerIO(word_bits = 8,
                                 channel_count = channel_count)
    }
    val io = new IO()

    // Just power the phy on right away, and route it any additional
    // configuration information it might care about.
    io.phy.power_on := Bool(true)

    // The channels are mostly independent, this deals with the mostly
    // part.
    for (channel <- 0 until channel_count) {
      val controller = Module(new Channel8b10bController)
      controller.io.phy <> io.phy.channels(channel)
      controller.io.ctl <> io.ctl.channels(channel)
      controller.io.on  := io.phy.powered
    }
  }

  // Each channel of the phy is mostly independent.  One of these
  // modules is constructed to handle each of the channels.
  class Channel8b10bController extends Module {
    class IO extends Bundle {
      val phy = new PhyChannel(encoded_word_bits = 10)
      val ctl = new SerialChannel(word_bits = 8)
      val on  = Bool(INPUT)
    }
    val io = new IO

    // Tx
    val enc = Module(new Encoder8b10b)
    enc.io.decoded := io.ctl.tx.bits
    io.phy.tx_data := enc.io.encoded

    // Rx
    val dec = Module(new Decoder8b10b)
    dec.io.encoded := io.phy.rx_data
    io.ctl.rx.bits := dec.io.decoded
  }
}

// Test cases for the 8b/10b encoder/decoder, running in a loopback
// mode.
package SerialTests {
  class Serial8b10bControllerLoopback extends Module {
    val channel_count = 8

    class IO extends Bundle {
      val i = Vec.fill(channel_count){UInt(INPUT,  width = 8)}
      val o = Vec.fill(channel_count){UInt(OUTPUT, width = 8)}
    }
    val io = new IO

    val serial = Module(new Serial.Serial8b10bController(channel_count))
    serial.io.ctl.reset := Bool(false)
    serial.io.ctl.config.clock_divider := UInt(0)

    for (i <- 0 until channel_count) {
      serial.io.ctl.channels(i).tx.bits := io.i(i)

      serial.io.phy.powered := Bool(true)
      serial.io.phy.channels(i).rx_data := serial.io.phy.channels(i).tx_data

      io.o(i) := serial.io.ctl.channels(i).rx.bits
    }
  }

  class Serial8b10bControllerTester(dut: Serial8b10bControllerLoopback) extends Tester(dut) {
    for (t <- 0 until (1 << 10)) {
      val i = (0 until 8).map{ i => (i, BigInt(8, rnd)) }

      i.foreach{ case(i, v) => poke(dut.io.i(i), v) }
      step(1)
      i.foreach{ case(i, v) => require(peek(dut.io.o(i)) == v) }
    }
  }

  object Serial8b10bControllerTester {
    def main(args: Array[String]): Unit = {
      chiselMainTest(args,
                     () => Module(new Serial8b10bControllerLoopback()))
      { dut => new Serial8b10bControllerTester(dut) }
    }
  }
}
