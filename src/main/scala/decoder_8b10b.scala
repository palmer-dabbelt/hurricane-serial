import Chisel._

package Serial {
  // A single 8b/10b encoder/decoder, attached to the phy (for actual
  // transmission) and to the rest of the chip (via the "controller"
  // IO bundle).
  class Decoder8b10bIO extends Bundle {
    val decoded = Bits(OUTPUT, width = 8)
    val encoded = Bits(INPUT,  width = 10)
    val control = Bool(OUTPUT)
    val valid   = Bool(OUTPUT)
  }

  class Decoder8b10b extends Module {
    val io = new Decoder8b10bIO()

    // Generates lookup tables that do the encoding, by turning the
    // strings I copied from Wikipedia to Vec[Bits], and then
    // generating a ROM.  This ROM is yet another union: the top bit
    // is "valid", which outputs whether or not this is a valid
    // codeword, the next bit is "kind", which is "1" when this is a
    // data word, and the remaining bits are the data.
    private def generate_lookup(description: Seq[String], prefix: String) = {
      val table_depth_bits = description(0).split(" +")(2).trim.size
      val table_width_bits = description(0).split(" +")(1).trim.size

      def description_to_pairs(listing: Seq[String]): Map[Int, UInt] = {
        listing
        .map(_.split(" +"))
        .map(vec => vec.map(element => element.trim))
        .map(vec => vec.size match {
          case 3 => Seq(Seq(vec(1), vec(2)))
          case 4 => Seq(Seq(vec(1), vec(2)), Seq(vec(1), vec(3)))
          case 6 => Seq(Seq(vec(1), vec(2)), Seq(vec(1), vec(3)),
                        Seq(vec(1), vec(4)), Seq(vec(1), vec(5)))
                      })
        .flatten
        .map(v => Seq(v(1), v(0)))
        .map(v => Seq(v(0), prefix + v(1)))
        .map(_ match { case Seq(a, b) => (Integer.parseInt(a, 2), UInt("b" + b)) })
        .toMap
      }

      val map = (
        description_to_pairs(description)
      ).withDefault( i => UInt(0) )

      val set = (0 until (1 << (table_depth_bits))).map{
        i => map(i)
      }

      Vec(set)
    }
    val lookup_6b5b_d = generate_lookup(Consts8b10b.mapping_5b6b, "1")
    val lookup_6b5b_c = generate_lookup(Consts8b10b.control_5b6b, "1")
    val lookup_4b3b_d = generate_lookup(Consts8b10b.mapping_3b4b, "1")
    val lookup_4b3b_c = generate_lookup(Consts8b10b.control_3b4b, "1")

    // These signal names match the Wikipedia entry, but none of the
    // other ones do.
    val abcdei = io.encoded(9, 4)
    val fgjh   = io.encoded(3, 0)

    // The lookups are actually a bit complicated here: each half of
    // the encoded word goes into two tables, one to decode data words
    // and the other to decode the control words.
    val vEDCBAd = lookup_6b5b_d(abcdei)
    val vEDCBAc = lookup_6b5b_c(abcdei)
    val vHGFd   = lookup_4b3b_d(fgjh)
    val vHGFc   = lookup_4b3b_c(fgjh)

    // Each of the lookup tables actually has an extra bit attached to
    // the top that is TRUE whenever the encoded part-word matches in
    // that lookup table.
    val lo_c = vEDCBAc(5)
    val lo_d = vEDCBAd(5)
    val hi_c = vHGFc(3)
    val hi_d = vHGFd(3)

    // 
    io.valid   := Bool(false)
    io.control := Bool(false)
    io.decoded := UInt(0)
    when (lo_c && hi_c) {
      io.valid   := Bool(true)
      io.control := Bool(true)
      io.decoded := Cat(vHGFc(2, 0), vEDCBAc(4, 0))
    }
    when (lo_d && hi_d) {
      io.valid   := Bool(true)
      io.control := Bool(false)
      io.decoded := Cat(vHGFd(2, 0), vEDCBAd(4, 0))
    }
  }
}

// Tests for the 8b10b decoder.  This simply uses the assumed good
// encoder to check to make sure that every data word can actually be
// decoded, nothing else.
package SerialTests {
  class Decoder8b10bLoopbackIO extends Bundle {
    val i = UInt(INPUT,  width = 8)
    val e = UInt(OUTPUT, width = 10)
    val o = UInt(OUTPUT, width = 8)
    val c = UInt(OUTPUT, width = 1)
    val v = UInt(OUTPUT, width = 1)
  }

  class Decoder8b10bLoopback extends Module {
    val io = new Decoder8b10bLoopbackIO

    private val encoder = Module(new Serial.Encoder8b10b)
    private val decoder = Module(new Serial.Decoder8b10b)

    encoder.io.decoded := io.i
    decoder.io.encoded := encoder.io.encoded
    io.e := encoder.io.encoded
    io.o := decoder.io.decoded
    io.v := decoder.io.valid
    io.c := decoder.io.control
  }

  class Decoder8b10bTester(dut: Decoder8b10bLoopback) extends Tester(dut) {
    for (t <- 0 until (1 << 19)) {
      val i = BigInt(8, rnd)
      poke(dut.io.i, i)
      step(1)
      peek(dut.io.e)
      require(peek(dut.io.o) == i)
      require(peek(dut.io.v) == 1)
      require(peek(dut.io.c) == 0)
    }
  }

  object Decoder8b10bTester {
    def main(args: Array[String]): Unit = {
      chiselMainTest(args,
                     () => Module(new Decoder8b10bLoopback()))
      { dut => new Decoder8b10bTester(dut) }
    }
  }
}
