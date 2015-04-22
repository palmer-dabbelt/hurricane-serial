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
    io.ctl.tx.ready := Bool(true)
    when (io.ctl.tx.valid === Bool(false) && enc.io.balance === UInt(0)) {
      io.phy.tx_data := UInt("b1101111100")
    }
    when (io.ctl.tx.valid === Bool(false) && enc.io.balance === UInt(1)) {
      io.phy.tx_data := UInt("b0010110000")
    }

    // Rx
    val dec = Module(new Decoder8b10b)
    dec.io.encoded := io.phy.rx_data
    io.ctl.rx.bits := dec.io.decoded
    io.ctl.rx.valid := dec.io.valid && !dec.io.control
  }
}

// Test cases for the 8b/10b encoder/decoder, running in a loopback
// mode.
package SerialTests {
  class Serial8b10bControllerLoopback extends Module {
    val channel_count = 8
    val decoded_word_bits = 8
    val encoded_word_bits = 10

    class IO extends Bundle {
      val tx_ready = Vec.fill(channel_count){ Bool(OUTPUT) }
      val tx_valid = Vec.fill(channel_count){ Bool(OUTPUT) }
      val tx_count = Vec.fill(channel_count){ UInt(OUTPUT, width = 32) }
      val tx_data  = Vec.fill(channel_count){ UInt(OUTPUT, width = 8) }
      val tx_enc   = Vec.fill(channel_count){ UInt(OUTPUT, width = 10) }
      val rx_ready = Vec.fill(channel_count){ Bool(OUTPUT) }
      val rx_valid = Vec.fill(channel_count){ Bool(OUTPUT) }
      val rx_count = Vec.fill(channel_count){ UInt(OUTPUT, width = 32) }
      val rx_data  = Vec.fill(channel_count){ UInt(OUTPUT, width = 8) }
      val rx_enc   = Vec.fill(channel_count){ UInt(OUTPUT, width = 10) }
      val pass = Bool(OUTPUT)
      val sync = Bool(OUTPUT)
    }
    val io = new IO

    val serial = Module(new Serial.Serial8b10bController(channel_count))
    serial.io.ctl.reset := Bool(false)
    serial.io.ctl.config.clock_divider := UInt(1)

    val gen = Module(new Serial.WordGenerator(channel_count, decoded_word_bits))
    val ver = Module(new Serial.WordVerifier( channel_count, decoded_word_bits))

    val tx_count = Vec.fill(channel_count){ Reg(init = UInt(0, width = 32)) }
    val rx_count = Vec.fill(channel_count){ Reg(init = UInt(0, width = 32)) }

    for (i <- 0 until channel_count) {
      serial.io.ctl.channels(i).tx <> gen.io.tx(i)

      serial.io.phy.powered := Bool(true)
      serial.io.phy.channels(i).rx_data := serial.io.phy.channels(i).tx_data

      serial.io.ctl.channels(i).rx <> ver.io.rx(i)

      // Everything below here is for debugging
      when (serial.io.ctl.channels(i).tx.ready && gen.io.tx(i).valid) {
        tx_count(i) := tx_count(i) + UInt(1)
      }

      when (serial.io.ctl.channels(i).rx.valid && ver.io.rx(i).ready) {
        rx_count(i) := rx_count(i) + UInt(1)
      }

      io.tx_ready(i) := serial.io.ctl.channels(i).tx.ready
      io.tx_valid(i) := gen.io.tx(i).valid
      io.tx_count(i) := tx_count(i)
      io.tx_data(i)  := gen.io.tx(i).bits
      io.tx_enc(i)   := serial.io.phy.channels(i).tx_data

      io.rx_ready(i) := ver.io.rx(i).ready
      io.rx_valid(i) := serial.io.ctl.channels(i).rx.valid
      io.rx_count(i) := rx_count(i)
      io.rx_data(i)  := serial.io.ctl.channels(i).rx.bits
      io.rx_enc(i)   := serial.io.phy.channels(i).rx_data
    }

    val pass = Reg(init = Bool(true))
    val sync = Reg(init = Bool(false))
    when (ver.io.pass === Bool(false)) { pass := Bool(false) }
    when (ver.io.sync === Bool(true)) { sync := Bool(true) }
    io.pass := pass
    io.sync := sync
  }

  class Serial8b10bControllerTester(dut: Serial8b10bControllerLoopback) extends Tester(dut) {
    for (t <- 0 until (1 << 20)) {
      step(1)
      (0 until 8).map{ i => peek(dut.io.tx_ready(i)) }
      (0 until 8).map{ i => peek(dut.io.tx_valid(i)) }
      (0 until 8).map{ i => peek(dut.io.tx_count(i)) }
      (0 until 8).map{ i => peek(dut.io.tx_data(i)) }
      (0 until 8).map{ i => peek(dut.io.tx_enc(i)) }
      (0 until 8).map{ i => peek(dut.io.rx_ready(i)) }
      (0 until 8).map{ i => peek(dut.io.rx_valid(i)) }
      (0 until 8).map{ i => peek(dut.io.rx_count(i)) }
      (0 until 8).map{ i => peek(dut.io.rx_data(i)) }
      (0 until 8).map{ i => peek(dut.io.rx_enc(i)) }
      require(peek(dut.io.pass) == 1)
    }
    require(peek(dut.io.pass) == 1)
    require(peek(dut.io.sync) == 1)
  }

  object Serial8b10bControllerTester {
    def main(args: Array[String]): Unit = {
      chiselMainTest(args,
                     () => Module(new Serial8b10bControllerLoopback()))
      { dut => new Serial8b10bControllerTester(dut) }
    }
  }
}
