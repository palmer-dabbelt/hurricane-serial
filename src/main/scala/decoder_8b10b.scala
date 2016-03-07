import Chisel._
import scala.collection.mutable.HashMap

package Serial {
  // A single 8b/10b encoder/decoder, attached to the phy (for actual
  // transmission) and to the rest of the chip (via the "controller"
  // IO bundle).
  class Decoder8b10bIO extends Bundle {
    val decoded = Bits(OUTPUT, width = 8)
    val encoded = Bits(INPUT,  width = 10)
    val control = Bool(OUTPUT)
    val valid   = Bool(OUTPUT)
    val rd      = Bool(INPUT)
  }

  class Decoder8b10b extends Module {
    val io = new Decoder8b10bIO()

    // generates an inverse lookup table (except for alt codes)
    //  input is of the format {encoded,rd} (n bits + 1 bit)
    //  output is of the format {decoded,valid} (n-1 bits + 1 bit)
    private def generate_lookup(desc: Seq[String], n: Int): Vec[UInt] = {
      val map = new HashMap[Int,Int]
      desc.map(_.split(" +"))
          .map(seq => seq.map(element => element.trim))
          .map(seq => seq.size match {
            case 3 => {
              val enc    = Integer.parseInt(seq(2),2) << 1
              map(enc)   = Integer.parseInt(seq(1),2)
              map(enc+1) = Integer.parseInt(seq(1),2)
            }
            case 4 => {
              val enc0  = Integer.parseInt(seq(2),2) << 1
              val enc1  = Integer.parseInt(seq(3),2) << 1 + 1
              map(enc0) = Integer.parseInt(seq(1),2)
              map(enc1) = Integer.parseInt(seq(1),2)
            }
            case 6 => {
              // XXX This is the same as 4, and the assumption is that
              // the latter two entries are just mirror images of the first two
              val enc0  = Integer.parseInt(seq(2),2) << 1
              val enc1  = Integer.parseInt(seq(3),2) << 1 + 1
              map(enc0) = Integer.parseInt(seq(1),2)
              map(enc1) = Integer.parseInt(seq(1),2)

              require(seq(2).reverse equals seq(4))
              require(seq(3).reverse equals seq(5))
            }
          })
      Vec.tabulate(1 << (n+1)) { i =>
        if (map contains i)
          UInt((map(i) << 1) + 1, width = n) else UInt(0, width = n)
      }
    }

    private def has_alt(sym: UInt): Bool = {
      Consts8b10b.mapping_3b4b
        .map(_.split(" +"))
        .map(seq => seq.map(element => element.trim))
        .filter(seq => seq.size match {
          case 3 => false
          case 4 => false
          case 6 => true
        }).foldLeft(Bool(false)) { (out,seq) =>
          out | (sym === UInt(s"b${seq(2)}")) | (sym === UInt(s"b${seq(3)}"))
        }
    }

    val lookup_6b5b_d  = generate_lookup(Consts8b10b.mapping_5b6b,6)
    val lookup_6b5b_c  = generate_lookup(Consts8b10b.control_5b6b,6)
    val lookup_4b3b_d  = generate_lookup(Consts8b10b.mapping_3b4b,4)
    val lookup_4b3b_c  = generate_lookup(Consts8b10b.control_3b4b,4)

    // These signal names match the Wikipedia entry, but none of the
    // other ones do.
    val abcdei = io.encoded(9, 4)
    val fgjh   = io.encoded(3, 0)

    val new_rd = io.rd ^ (PopCount(abcdei) =/= PopCount(~abcdei))

    // The lookups are actually a bit complicated here: each half of
    // the encoded word goes into two tables, one to decode data words
    // and the other to decode the control words.
    val vEDCBAd = lookup_6b5b_d(Cat(abcdei,io.rd))
    val vEDCBAc = lookup_6b5b_c(Cat(abcdei,io.rd))

    // This assumes that the alternate encodings exist for codes ending in
    // run lengths of two
    val use_alt = abcdei(1,0) === Cat(~new_rd,~new_rd) && has_alt(fgjh)
    val alt_fgjh = Mux(use_alt,Cat(fgjh(0),fgjh(1),fgjh(2),fgjh(3)),fgjh)

    val vHGFd   = lookup_4b3b_d(Cat(alt_fgjh,new_rd))
    val vHGFc   = lookup_4b3b_c(Cat(alt_fgjh,new_rd))

    // Each of the lookup tables actually has an extra bit attached to
    // the top that is TRUE whenever the encoded part-word matches in
    // that lookup table.
    val lo_c = vEDCBAc(0)
    val lo_d = vEDCBAd(0)
    val hi_c = vHGFc(0)
    val hi_d = vHGFd(0)

    io.valid   := Bool(false)
    io.control := Bool(false)
    io.decoded := UInt(0)
    when (lo_c && hi_c) {
      io.valid   := Bool(true)
      io.control := Bool(true)
      io.decoded := Cat(vHGFc(3, 1), vEDCBAc(4, 1))
    }
    when (lo_d && hi_d) {
      io.valid   := Bool(true)
      io.control := Bool(false)
      io.decoded := Cat(vHGFd(3, 1), vEDCBAd(4, 1))
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
