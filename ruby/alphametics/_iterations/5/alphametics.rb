require 'set'

module Alphametics
  def self.solve(puzzle)
    Equation.new(puzzle).solve
  end

  private

  class Equation
    def initialize(puzzle)
      @parts = puzzle.split(/\s*(?:\+|==)\s*/).map(&:reverse).reverse
      @edge_letters = puzzle.scan(/\b[A-Z]/)
      @max_part_length = @parts.map(&:length).max
      @columns = (0...@max_part_length).map do |i|
        Column.new(@parts.map { |p| p[i] })
      end
    end

    def solve
      permutations = [[Permutation.new({}), 0]]
      @columns.each do |col|
        new_permutations = []
        permutations.each do |(op, prev_carry)|
          op.each_next_permutation(col.unique_letters) do |np|
            if np.valid?(@edge_letters)
              carry = col.solve(np.mapping, prev_carry)
              if !carry.nil?
                new_permutations << [np, carry]
              end
            end
          end
        end
        permutations = new_permutations
      end
      permutations.empty? ? {} : permutations.first.first.mapping
    end
  end

  class Column
    attr_reader :parts

    def initialize(parts)
      @parts = parts
    end

    def unique_letters
      parts.compact.uniq
    end

    def solve(mapping, prev_carry)
      nums = parts.map { |p| p.nil? ? 0 : mapping[p] }
      carry, result = (nums.drop(1).sum + prev_carry).divmod(10)
      result == nums.first ? carry : nil
    end
  end

  class Permutation
    attr_reader :mapping

    def initialize(mapping)
      @mapping = mapping
    end

    def valid?(edge_letters)
      edge_letters.none? { |el| mapping[el] == 0 }
    end

    def each_next_permutation(letters)
      new_letters = letters - mapping.keys
      ((0..9).to_a - mapping.values).permutation(new_letters.size) do |p|
        new_mapping = mapping.merge(new_letters.zip(p).to_h)
        yield Permutation.new(new_mapping)
      end
    end
  end
end
