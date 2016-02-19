import Chisel._

package Serial {
  // A single, full-duplex channel as it appears on the phy.  These
  // words have already been encoded into whatever the transport
  // requires, so the phy just has to serialize/deserialize onto the
  // actual physical layer.  At this layer the channels are not
  // decoupled: on recieve the encoding has an idle pattern that the
  // controller detects, and there's an extra bit for the transmit
  // part that determines if the phy should transmit at that
  // particular cycle -- this is just a hint, the controller must set
  // transmit_data to an idle pattern.
  class PhyChannel(encoded_word_bits: Int) extends Bundle {
    val tx_enabled = Bool(OUTPUT)
    val tx_data    = Bits(OUTPUT, width=encoded_word_bits)
    val rx_data    = Bits(INPUT, width=encoded_word_bits)
  }

  // Contains all the information that's necessary to make the phy
  // work.  In addition to some configuration information that's
  // supposed to be generic for all phys (pretty much just the ability
  // to know when the device has powered on) there's a generic
  // configuration object that's phy-specific that ends up being
  // passed up to the top level.
  class PhyIO(encoded_word_bits: Int, channel_count: Int) {
    val power_on = Bool(OUTPUT)
    val powered  = Bool(INPUT)
    val reset    = Bool(OUTPUT)
    
    val channels = Vec.fill(channel_count){new PhyChannel(encoded_word_bits)}
  }

  // Wrap word_bits with an additional control bool to indicate a control symbol
  class SerialSymbol(word_bits: Int) extends Bundle {
    val bits = Bits(width = word_bits)
    val control = Bool()

    override def cloneType = new SerialSymbol(word_bits).asInstanceOf[this.type]
  }

  // Contains a single serial channel, which is a full-duplex pair of
  // decoupled words.
  class SerialChannel(word_bits: Int) extends Bundle {
    val tx = Decoupled(new SerialSymbol(word_bits)).flip()
    val rx = Decoupled(new SerialSymbol(word_bits))
  }

  // The control interface to the serial controller.  This itself
  // doesn't need much configuration itself because it doesn't do any
  // muxing, but it does pass through some configuration to the phy.
  class ControllerConfig extends Bundle {
    // The phy runs at a constant clock divider as compared to the
    // rest of the controller -- this is a bit of a misnomer, as most
    // of the phy is actually running faster than the controller is.
    // The clock relationship looks something like (SERIAL_BITRATE /
    // encoded_bits_per_word) / clock_divider = CORE_CLOCK.
    val clock_divider = UInt(INPUT, width=32)
  }

  // The interface between the controller and the rest of the system.
  class ControllerIO(word_bits: Int, channel_count: Int) extends Bundle {
    val reset      = Bool(INPUT)
    val config     = new ControllerConfig()
    val channels = Vec.fill(channel_count){new SerialChannel(word_bits)}
  }
}
