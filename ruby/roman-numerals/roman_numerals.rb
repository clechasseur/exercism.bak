class Integer
  def to_roman
    raise RuntimeError, "Roman numerals can only express 1..3999" unless (1..3999) === self
    digits.each.with_index.map { |d, i| digit_to_roman(d, NUMERAL_INFOS[i]) }.reverse.join
  end

  private

  NumeralInfo = Struct.new(:one, :five, :ten)

  NUMERAL_INFOS = [
    NumeralInfo.new("I", "V", "X"),
    NumeralInfo.new("X", "L", "C"),
    NumeralInfo.new("C", "D", "M"),
    NumeralInfo.new("M")
  ]

  def digit_to_roman(digit, numeral_info)
    case digit
    when 0...4
      numeral_info.one * digit
    when 4
      numeral_info.one + numeral_info.five
    when 5...9
      numeral_info.five + (numeral_info.one * (digit - 5))
    when 9
      numeral_info.one + numeral_info.ten
    else
      ""
    end
  end
end
