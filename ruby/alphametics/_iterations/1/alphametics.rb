require 'set'

module Alphametics
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

  def self.solve(puzzle)
    Puzzle.new(puzzle).each_permutation do |perm|
      return perm if eval(puzzle.gsub(/[A-Z]/, perm))
    end
    {}
  end
end
