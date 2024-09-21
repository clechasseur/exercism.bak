require 'set'

module Alphametics
  def self.solve(puzzle)
    Puzzle.new(puzzle).each_permutation do |perm|
      return perm if Equation.new(puzzle).eval?(perm)
    end
    {}
  end

  private

  class Puzzle
    def initialize(puzzle)
      @letters = Set.new
      @edge_letters = Set.new
      puzzle.scan(/[A-Z]+/) do |m|
        @letters.merge(m.chars)
        @edge_letters.add(m[0])
      end
    end

    def each_permutation
      (0..9).to_a.permutation(@letters.size) do |p|
        perm = @letters.zip(p).to_h
        yield perm if valid_permutation? perm
      end
    end

    private

    def valid_permutation?(perm)
      @edge_letters.none? { |el| perm[el] == 0 }
    end
  end

  class Equation
    def initialize(puzzle)
      left_side, @right_side = puzzle.split /\s*==\s*/
      @left_parts = left_side.split /\s*\+\s*/
    end

    def eval?(perm)
      @left_parts.map { |p| part_to_i(p, perm) }.sum == part_to_i(@right_side, perm)
    end

    private

    def part_to_i(part, perm)
      part.gsub(/[A-Z]/, perm).to_i
    end
  end
end
