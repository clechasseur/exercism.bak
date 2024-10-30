/**
 * Utilities related to bit manipulation.
 */
object BitUtils:
  /**
   * Returns the number of bits set in the given number.
   *
   * @param n number to count bits in
   * @return number of bits in n
   */
  def popCount(n: Int): Int = n match
    case 0 => 0
    case _ if (n & 1) != 0 => popCount(n >> 1) + 1
    case _ => popCount(n >> 1)

  /**
   * Returns the least-significant bit from the given number.
   * If the number has no bit set, returns 0.
   *
   * @note The return value is the bit, not its index. For example:
   *
   * {{{
   * var n = 0x6            // 0b0110
   * val bit = firstBit(n)  // bit == 0x2 (0b0010)
   * n &= ~bit              // n == 0x4 (0b0100)
   * }}}
   * @param n number to get first bit for
   * @return first bit in n, or 0 if n is 0
   */
  def firstBit(n: Int): Int = n match
    case 0 => 0
    case _ if (n & 1) != 0 => 1
    case _ => firstBit(n >> 1) << 1
