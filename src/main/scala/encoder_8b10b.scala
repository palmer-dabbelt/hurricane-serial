import Chisel._

package Serial {
  object Consts8b10b {
    val max_mapping_word_width = 4

    // This is the 8b10b encoding from Wikipedia, these tables are
    // copied directly from
    // <http://en.wikipedia.org/wiki/8b/10b_encoding>.
    val mapping_5b6b = 
      Seq("D.00 	00000 	100111 	011000",
          "D.01 	00001 	011101 	100010",
          "D.02 	00010 	101101 	010010",
          "D.03 	00011 	110001",
          "D.04 	00100 	110101 	001010",
          "D.05 	00101 	101001",
          "D.06 	00110 	011001",
          "D.07 	00111 	111000 	000111",
          "D.08 	01000 	111001 	000110",
          "D.09 	01001 	100101",
          "D.10 	01010 	010101",
          "D.11 	01011 	110100",
          "D.12 	01100 	001101",
          "D.13 	01101 	101100",
          "D.14 	01110 	011100",
          "D.15 	01111 	010111 	101000",
          "D.16 	10000 	011011 	100100",
          "D.17 	10001 	100011",
          "D.18 	10010 	010011",
          "D.19 	10011 	110010",
          "D.20 	10100 	001011",
          "D.21 	10101 	101010",
          "D.22 	10110 	011010",
          "D.23   	10111 	111010 	000101",
          "D.24 	11000 	110011 	001100",
          "D.25 	11001 	100110",
          "D.26 	11010 	010110",
          "D.27   	11011 	110110 	001001",
          "D.28 	11100 	001110",
          "D.29   	11101 	101110 	010001",
          "D.30   	11110 	011110 	100001",
          "D.31 	11111 	101011 	010100");

    val mapping_3b4b =
      Seq("D.x.0 	000 	1011 	0100",
          "D.x.1 	001 	1001",
          "D.x.2 	010 	0101",
          "D.x.3 	011 	1100 	0011",
          "D.x.4 	100 	1101 	0010",
          "D.x.5 	101 	1010",
          "D.x.6 	110 	0110",
          "D.x.7 	111 	1110 	0001    0111 1000")
  }

  // A single 8b/10b encoder/decoder, attached to the phy (for actual
  // transmission) and to the rest of the chip (via the "controller"
  // IO bundle).
  class Encoder8b10b extends Module {
    class IO extends Bundle {
      val decoded = Bits(INPUT,  width = 8)
      val encoded = Bits(OUTPUT, width = 10)
      val balance = Bool(OUTPUT)
    }
    val io = new IO()

    // Generates lookup tables that do the encoding, by turning the
    // strings I copied from Wikipedia to Vec[Bits] -- this is kind of
    // shitty, because Vec[Vec[UInt]] (which is what I really wanted)
    // doesn't generate efficient ROMs.
    def generate_lookup(description: Seq[String]) = {
      Vec(description
        .map(_.split(" +"))
        .map(vec => vec.map(element => element.trim))
        .map(vec => vec.size match {
          case 3 => Seq(vec(2), vec(2), vec(2), vec(2))
          case 4 => Seq(vec(2), vec(3), vec(2), vec(3))
          case 6 => Seq(vec(2), vec(3), vec(4), vec(5))
        })
        .map(vec => vec.map(element => UInt("b" + element)))
        .flatten
        )
    }
    val lookup_5b6b = generate_lookup(Consts8b10b.mapping_5b6b)
    val lookup_3b4b = generate_lookup(Consts8b10b.mapping_3b4b)

    // This helper function hides the exact indexing order from my
    // user below, so I don't have to have a huge line in there.
    def lookup(table: Vec[UInt], decoded_word: UInt, rd: UInt, run: UInt) = {
      table(decoded_word * UInt(Consts8b10b.max_mapping_word_width) +
            rd ^ UInt(1) +
            run * UInt(2)
          )
    }

    // Checks to see if there's the same number of 1's and 0's, or a
    // different number.
    def mismatched(value: UInt) = {
      PopCount(value) != PopCount(~value)
    }

    // Checks to see if the possibility of a long run length exists
    // for the given input/rd combination -- this presumes some amount
    // of encoding table information, so you can't change those
    // without changing this!
    def check_run(x: UInt, rd: UInt) = {
      val run = UInt(width = 1)
      run := UInt(0)
      when (rd === UInt(1)) { run := (x === UInt(11)) || (x === UInt(13)) || (x === UInt(14)) }
      when (rd === UInt(0)) { run := (x === UInt(17)) || (x === UInt(18)) || (x === UInt(20)) }
      run ^ UInt(1)
    }

    // This encodes the running disparity, where "0" means a RD of
    // "-1", and "1" means a RD of "1".
    val rd = Reg(init = UInt(0, width = 1))

    val EDCBA = io.decoded(4, 0)
    val HGF   = io.decoded(7, 5)

    val abcdei = lookup(lookup_5b6b, EDCBA, rd, UInt(0))
    val rd_after_abcdei = rd ^ mismatched(abcdei)
    val fgjh   = lookup(lookup_3b4b, HGF, rd_after_abcdei, check_run(EDCBA, rd_after_abcdei))

    val encoded = Cat(fgjh, abcdei)
    rd := rd ^ mismatched(encoded)

    io.encoded := encoded
    io.balance := rd
  }
}

// Tests for the 8b10b encoder, which attempts to ensure that the
// encoding meets some of the requirements of the encoding without
// actually depending on the actual instruction tables.
package SerialTests {
  import scala.collection.mutable.HashMap
  import scala.collection.mutable.MultiMap
  import scala.collection.mutable.Set

  class Encoder8b10bTester(dut: Serial.Encoder8b10b) extends Tester(dut) {
//    // Here's the one version that's defined at Wikipedia
//    poke(dut.io.decoded, BigInt("00111111", 2))
//    step(1)
//    require(peek(dut.io.encoded) == BigInt("1010111001", 2))

    // Here we just go ahead and encode a bunch of stuff, with the
    // goal that we eventually get good coverage.
    val d2e = new HashMap[BigInt, Set[BigInt]] with MultiMap[BigInt, BigInt]
    val e2d = new HashMap[BigInt, Set[BigInt]] with MultiMap[BigInt, BigInt]

    // Count the numbers of zeros and ones, to ensure they stay even.
    var zeroes = 1
    var ones   = 0

    // Contains the previous bit pattern, which for simplicity's sake
    // we assume starts by encoding 0x00.
    var prev_bits = ""

    for (t <- 0 until (1 << 17)) {
      val decoded = BigInt(8, rnd)
      poke(dut.io.decoded, decoded)
      step(1)
      val encoded = peek(dut.io.encoded)

      // Check to make sure the code is almost a bijection (there can
      // be up to two encoded values for each decoded value).
      d2e.addBinding(decoded, encoded)
      require(d2e.get(decoded).get.size <= 2)
      e2d.addBinding(encoded, decoded)
      require(e2d.get(encoded).get.size == 1)

      // Ensure that there's a DC line balance, which is the whole
      // point of 8b/10b.
      for (i <- 0 until 10) {
        if (i < encoded.bitLength && encoded.testBit(i))
          ones += 1
        else
          zeroes += 1
      }
      require(math.abs(zeroes - ones) < 2)
      if (zeroes > ones)
        require(peek(dut.io.balance) == 0)
      else
        require(peek(dut.io.balance) == 1)

      // Another property of the encoded bitstream is that there is a
      // limit of 5 consecutive bits of the same value
      def encoded_to_bitstring(encoded: BigInt): String = {
        (0 until 10).map{i =>
          if (i < encoded.bitLength && encoded.testBit(i))
            "1"
          else
            "0"
        }.mkString("")
      }

      def string_trunc_or_dont(in: String, end: Int): String = {
        if (in.length <= end)
          return in
        else
          return in.substring(0, end)
      }

      prev_bits = prev_bits + encoded_to_bitstring(encoded)
      prev_bits = string_trunc_or_dont(prev_bits, 99) // 99 is
                                                      // arbitrary,
                                                      // but hopefully
                                                      // safe
      require(prev_bits.contains("000000") == false)
      require(prev_bits.contains("111111") == false)

      // The additional logic inside check_run() should ensure that
      // these patterns also can't exist.
      require(prev_bits.contains("0011111") == false)
      require(prev_bits.contains("1100000") == false)
    }
  }

  object Encoder8b10bTester {
    def main(args: Array[String]): Unit = {
      chiselMainTest(args,
                     () => Module(new Serial.Encoder8b10b()))
      { dut => new Encoder8b10bTester(dut) }
    }
  }
}
