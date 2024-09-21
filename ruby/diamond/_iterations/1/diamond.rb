module Diamond
  def self.make_diamond(letter)
    return "A\n" if letter == "A"

    line_size = (letter.ord - "A".ord) * 2 + 1
    ([*("A".ord..letter.ord)] + [*(letter.ord - 1).downto("A".ord)]).map do |l|
      make_diamond_line(l.chr, line_size)
    end.join
  end

  def self.make_diamond_line(letter, line_size)
    if letter == "A"
      outer_whitespace = " " * ((line_size - 1) / 2)
      "#{outer_whitespace}#{letter}#{outer_whitespace}\n"
    else
      inner_size = (letter.ord - "A".ord) * 2 - 1
      outer_size = (line_size - inner_size - 2) / 2
      inner_whitespace = " " * inner_size
      outer_whitespace = " " * outer_size
      "#{outer_whitespace}#{letter}#{inner_whitespace}#{letter}#{outer_whitespace}\n"
    end
  end
end
