import Chisel._

package Serial {
  // A plain LFSR -- this doesn't self-synchronize, as it's designed
  // to make sure each and every word gets through and I don't want
  // anything muddling that up.
  class LFSR(bit_width: Int) extends Module {
    class IO extends Bundle {
      val bits         = Bits(OUTPUT, width = bit_width)
      val increment    = Bool(INPUT)
    }
    val io = new IO

    val r = Reg(init = UInt(3184 & ((1 << bit_width) - 1), width = bit_width))
    io.bits := r

    // The meat of the LFSR is actually a prime number generator,
    // which is implemented here in a super inefficient way.
    def prime_p(i: Int): Boolean = {
      if (i <= 1)
        false
      else if (i == 2)
        true
      else
        !(2 to (i - 1)).exists(x => i % x == 0)
    }
    val prime_stream = 2 #:: Stream.from(3,2).filter(prime_p)
    val primes = prime_stream.takeWhile(_ <= bit_width).toArray

    // The final part is just an XOR reduction of the prime-indexed
    // bits inside the register, with an extra check to make sure
    // there's an even number of primes (which is required to get a
    // good polynomial).
    val bits = Vec(primes.map{r.toBools(_)})
    val lo_bit = bits.toBits.xorR ^ (if (primes.size % 2 == 0) Bool(false) else r(0))

    when (io.increment) { r := Cat(lo_bit, r(bit_width-1, 1)) }
  }

  // Generates a stream of random bits that are sometimes actually
  // valid.
  class WordGenerator(channel_count: Int, word_bits: Int)
  extends Module {
    class IO extends Bundle {
      val tx = Vec.fill(channel_count){ Decoupled(Bits(width = word_bits)) }
    }
    val io = new IO

    for (i <- 0 until channel_count) {
      // Generates a random sequence that sometimes spits out a valid
      // data item -- this tests the decoupled IO part.
      val valid = Module(new LFSR(20))
      valid.io.increment := Bool(true)
      io.tx(i).valid := valid.io.bits(5)

      val reg = Module(new LFSR(word_bits))
      io.tx(i).bits := reg.io.bits

      // In order to stay synchronized with the reciever we need to
      // only bump the data LFSR when something is actually sent.
      reg.io.increment := (valid.io.bits(5) & io.tx(i).ready)
    }
  }

  // Checks a stream of words to make sure it matches with 
  class WordVerifier(channel_count: Int, word_bits: Int)
  extends Module {
    class IO extends Bundle {
      val rx   = Vec.fill(channel_count){ Decoupled(Bits(width = word_bits)).flip }
      val pass = Bool(OUTPUT)
      val sync = Bool(OUTPUT)
    }
    val io = new IO

    // The default state is to pass -- we only fail when there's an
    // input that doesn't match.
    io.pass := Bool(true)
    io.sync := Bool(true)

    for (i <- 0 until channel_count) {
      // We're always ready to accept input
      val ready = Module(new LFSR(20))
      ready.io.increment := Bool(true)
      io.rx(i).ready := ready.io.bits(10)

      // This LFSR is synchronized with the transmitter because it
      // starts at the same value and only increments when a value
      // shows up.
      val reg = Module(new LFSR(word_bits))
      reg.io.increment := (io.rx(i).valid & ready.io.bits(10))

      when (io.rx(i).ready & (reg.io.bits != io.rx(i).bits)) {
        io.pass := Bool(false)
      }

      when (reg.io.bits != io.rx(i).bits) {
        io.sync := Bool(false)
      }
    }
  }
}

// A test that ensures that tests that should pass actually do when
// exposed to the generator.
package SerialTests {
  class WordLoopback(channel_count: Int, word_bits: Int) extends Module {
    class IO extends Bundle {
      val passing      = Bool(OUTPUT)
      val synchronized = Bool(OUTPUT)
      val sent         = Vec.fill(channel_count){ UInt(OUTPUT, width = 32) }
    }
    val io = new IO

    val passing      = Reg(init = Bool(true))
    io.passing := passing
    val synchronized = Reg(init = Bool(false))
    io.synchronized := synchronized

    // Simply loop together a generator and a verifier.
    val word_generator = Module(new Serial.WordGenerator(channel_count, word_bits))
    val word_verifier  = Module(new Serial.WordVerifier( channel_count, word_bits))
    word_generator.io.tx <> word_verifier.io.rx

    when (word_verifier.io.pass === Bool(false)) {
      passing := Bool(false)
    }
    when (word_verifier.io.sync === Bool(true)) {
      synchronized := Bool(true)
    }

    // Count the number of sent words, to ensure something went over
    // the line.
    val sent = Vec.fill(channel_count){ Reg(init = UInt(0, width = 32)) }
    for (i <- 0 until channel_count) {
      when (word_generator.io.tx(i).valid && word_verifier.io.rx(i).ready) {
        sent(i) := sent(i) + UInt(1)
      }
      io.sent(i) := sent(i)
    }
  }

  class PassingWordLoopbackTester(dut: SerialTests.WordLoopback, channel_count: Int)
  extends Tester(dut) {
    val iterations = (1 << 16)

    for (t <- 0 until iterations) {
      step(1)
      (0 until channel_count).map{ i => peek(dut.io.sent(i)) }
      require(peek(dut.io.passing) == 1)
    }

    require(peek(dut.io.synchronized) == 1)
    (0 until channel_count).map{ i => require(peek(dut.io.sent(i)) > (iterations / 16)) }
  }

  object PassingWordLoopbackTester {
    def main(args: Array[String]): Unit = {
      // This default configuration matches the Hurricane serial
      // configuration, which is why it's the one we're testing.
      val channel_count     = 8
      val word_bits         = 32

      chiselMainTest(args,
                     () => Module(new WordLoopback(channel_count,
                                                   word_bits)
                                ))
      { dut => new PassingWordLoopbackTester(dut, channel_count) }
    }
  }
}

// A test that makes sure the verifier will fail
package SerialTests {
  class BrokenWordLoopback(channel_count: Int, word_bits: Int) extends Module {
    class IO extends Bundle {
      val passing      = Bool(OUTPUT)
      val synchronized = Bool(OUTPUT)
    }
    val io = new IO

    val passing      = Reg(init = Bool(true))
    io.passing := passing
    val synchronized = Reg(init = Bool(false))
    io.synchronized := synchronized

    // Simply loop together a generator and a verifier.
    val word_generator = Module(new Serial.WordGenerator(channel_count, word_bits))
    val word_verifier  = Module(new Serial.WordVerifier( channel_count, word_bits))
    word_generator.io.tx <> word_verifier.io.rx

    when (word_verifier.io.pass === Bool(false)) {
      passing := Bool(false)
    }
    when (word_verifier.io.sync === Bool(true)) {
      synchronized := Bool(true)
    }

    // This line breaks the loopback test
    word_verifier.io.rx(channel_count / 2).bits := word_generator.io.tx(channel_count / 2).bits | UInt(1 << (word_bits / 2))
  }

  class FailingWordLoopbackTester(dut: SerialTests.WordLoopback, channel_count: Int)
  extends Tester(dut) {
    for (t <- 0 until 10240) {
      step(1)
    }

    require(peek(dut.io.passing) == 0)
    require(peek(dut.io.synchronized) == 1)
  }

  object FailingWordLoopbackTester {
    def main(args: Array[String]): Unit = {
      // This default configuration matches the Hurricane serial
      // configuration, which is why it's the one we're testing.
      val channel_count     = 8
      val word_bits         = 32

      chiselMainTest(args,
                     () => Module(new WordLoopback(channel_count,
                                                   word_bits)
                                ))
      { dut => new PassingWordLoopbackTester(dut, channel_count) }
    }
  }
}
