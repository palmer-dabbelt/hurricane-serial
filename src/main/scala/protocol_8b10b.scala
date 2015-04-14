import Chisel._

package Serial {
  // A single 8b/10b encoder/decoder, attached to the phy (for actual
  // transmission) and to the rest of the chip (via the "controller"
  // IO bundle).
  class Serial8b10bController(channel_count: Int, phy_config_bundle: Bundle)
  extends Module {
    class IO extends Bundle {
      val phy = new PhyIO(encoded_word_bits = 10,
                          channel_count = channel_count,
                          config_bundle = phy_config_bundle)
      val controller = new ControllerIO(8,
                                        channel_count = channel_count,
                                        phy_config_bundle = phy_config_bundle)
    }
    val io = new IO()
  }
}
