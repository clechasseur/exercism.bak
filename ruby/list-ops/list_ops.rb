module ListOps
  def self.reducer(ary, memo, &block)
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
    reducer(ary, [], &:unshift)
  end

  def self.concatter(*ary)
    reducer(ary, []) do |acc, a|
      reducer(a, acc, &:<<)
    end
  end

  def self.mapper(ary)
    reducer(ary, []) { |acc, e| acc << (yield e) }
  end

  def self.filterer(ary)
    reducer(ary, []) { |acc, e| acc << e if yield e; acc }
  end

  def self.sum_reducer(ary)
    reducer(ary, 0, &:+)
  end

  def self.factorial_reducer(ary)
    reducer(ary, 1, &:*)
  end
end
