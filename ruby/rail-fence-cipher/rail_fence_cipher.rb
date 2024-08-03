module RailFenceCipher
  def self.encode(input, num_rails)
    rails = Rails.new(num_rails)
    lines = Array.new(num_rails) { [] }
    input.chars.zip(rails.rail_indexes(input.length)).each do |c, rail_index|
      lines[rail_index] << c
    end
    lines.join
  end

  def self.decode(output, num_rails)
    rails = Rails.new(num_rails)
    indexes = rails.rail_indexes(output.length)
    lines = output.chars.zip(indexes.sort).chunk(&:last).map do |_, line|
      line.map(&:first)
    end
    indexes.map { |rail_index| lines[rail_index].shift }.join
  end

  private

  class Rails
    attr_reader :num

    def initialize(num)
      @num = num
    end

    def rail_indexes(n)
      return Array.new(n) { 0 } if num == 1

      Enumerator.produce([0, 1]) do |index, dir|
        dir = -dir unless (0...num) === index + dir
        [index + dir, dir]
      end.take(n).map(&:first)
    end
  end
end
