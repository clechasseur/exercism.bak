class Proverb
  def initialize(*input, qualifier: "")
    @input = input
    @qualifier = qualifier
  end

  def to_s
    @input.zip(@input.drop(1)).map(&method(:line)).join("\n")
  end

  private

  def line((want, lost))
    return last_line if lost.nil?
    "For want of a #{want} the #{lost} was lost."
  end

  def last_line
    "And all for the want of a #{@qualifier}#{@qualifier.empty? ? '' : ' '}#{@input.first}."
  end
end
