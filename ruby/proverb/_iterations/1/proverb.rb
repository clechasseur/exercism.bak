class Proverb
  def initialize(*input, qualifier: "")
    @input = input
    @qualifier = qualifier
  end

  def to_s
    (@input.zip(@input.drop(1)).map do |want, lost|
      "For want of a #{want} the #{lost} was lost."
    end.take(@input.size - 1) + [last_line]).join("\n")
  end

  private

  def last_line
    "And all for the want of a #{@qualifier}#{@qualifier.empty? ? '' : ' '}#{@input.first}."
  end
end
