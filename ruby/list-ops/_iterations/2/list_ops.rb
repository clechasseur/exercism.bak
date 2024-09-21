module ListOps
  def self.reducer(ary, memo)
    acc = memo
    for e in ary
      acc = yield acc, e
    end
    acc
  end

  def self.arrays(ary)
    reducer(ary, 0) { |acc, *| acc += 1 }
  end

  def self.reverser(ary)
    reducer(ary, []) { |acc, e| acc.unshift e }
  end

  def self.concatter(*ary)
    reducer(ary, []) do |acc, a|
      reducer(a, acc) { |iacc, e| iacc << e }
    end
  end

  def self.mapper(ary)
    reducer(ary, []) { |acc, e| acc << (yield e) }
  end

  def self.filterer(ary)
    reducer(ary, []) { |acc, e| acc << e if yield e; acc }
  end

  def self.sum_reducer(ary)
    reducer(ary, 0) { |acc, e| acc += e }
  end

  def self.factorial_reducer(ary)
    reducer(ary, 1) { |acc, e| acc *= e }
  end
end
