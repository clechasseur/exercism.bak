class Proverb
  def initialize(*input, qualifier: "")
    @input = input
    @qualifier = qualifier
  end

  def to_s
    ((0...(@input.size - 1)).map { |n| line(n) } << last_line).join("\n")
  end

  private

  def line(n)
    "For want of a #{@input[n]} the #{@input[n + 1]} was lost."
  end

  def last_line
    "And all for the want of a #{@qualifier}#{@qualifier.empty? ? '' : ' '}#{@input.first}."
  end
end
