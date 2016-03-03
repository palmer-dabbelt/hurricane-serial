import Chisel._

package Serial {
  // Constants required to make an 8b/10b codec.
  object Consts8b10b {
    val max_mapping_word_width = 4

    // This is the 8b10b encoding from Wikipedia, these tables are
    // copied directly from
    // <http://en.wikipedia.org/wiki/8b/10b_encoding>.
    val mapping_5b6b = 
      Seq("D.00     00000   100111  011000",
          "D.01     00001   011101  100010",
          "D.02     00010   101101  010010",
          "D.03     00011   110001",
          "D.04     00100   110101  001010",
          "D.05     00101   101001",
          "D.06     00110   011001",
          "D.07     00111   111000  000111",
          "D.08     01000   111001  000110",
          "D.09     01001   100101",
          "D.10     01010   010101",
          "D.11     01011   110100",
          "D.12     01100   001101",
          "D.13     01101   101100",
          "D.14     01110   011100",
          "D.15     01111   010111  101000",
          "D.16     10000   011011  100100",
          "D.17     10001   100011",
          "D.18     10010   010011",
          "D.19     10011   110010",
          "D.20     10100   001011",
          "D.21     10101   101010",
          "D.22     10110   011010",
          "D.23     10111   111010  000101",
          "D.24     11000   110011  001100",
          "D.25     11001   100110",
          "D.26     11010   010110",
          "D.27     11011   110110  001001",
          "D.28     11100   001110",
          "D.29     11101   101110  010001",
          "D.30     11110   011110  100001",
          "D.31     11111   101011  010100");

    val mapping_3b4b =
      Seq("D.x.0    000     1011    0100",
          "D.x.1    001     1001",
          "D.x.2    010     0101",
          "D.x.3    011     1100    0011",
          "D.x.4    100     1101    0010",
          "D.x.5    101     1010",
          "D.x.6    110     0110",
          "D.x.7    111     1110    0001    0111 1000")

    val control_5b6b =
      Seq("K.28     11100   001111  110000")

    val control_3b4b =
      Seq("K.x.0    000     1011    0100",
          "K.x.1    001     0110    1001",
      //  "K.x.2    010     1010    0101",
          "K.x.3    011     1100    0011",
          "K.x.4    100     1101    0010",
          "K.x.5    101     0101    1010"
      //  "K.x.6    110     1001    0110",
      //  "K.x.7    111     0111    1000"
	  )

    // decoded control symbols
    val K_28_0 = UInt("b00011100")
    val K_28_1 = UInt("b00111100")
    //val K_28_2 = UInt("b01011100")
    val K_28_3 = UInt("b01111100")
    val K_28_4 = UInt("b10011100")
    val K_28_5 = UInt("b10111100")
    //val K_28_6 = UInt("b11011100")
    // K.28.7 is illegal in this implementation
    // Pick one of the two commas (the other being K.28.1)
    // This particular comma is better with clock recovery because it has more transitions
    val COMMA  = this.K_28_5
    val COMMA_ENC_0 = UInt("b0011111010")
    val COMMA_ENC_1 = ~COMMA_ENC_0
  }
}
