module ListOps
  def self.arrays(ary)
    size = 0
    for _ in ary
      size += 1
    end
    size
  end

  def self.reverser(ary)
    rev = []
    for e in ary
      rev.unshift e
    end
    rev
  end

  def self.concatter(*ary)
    con = []
    for a in ary
      for e in a
        con << e
      end
    end
    con
  end

  def self.mapper(ary)
    mapd = []
    for e in ary
      mapd << (yield e)
    end
    mapd
  end

  def self.filterer(ary)
    filtd = []
    for e in ary
      filtd << e if yield e
    end
    filtd
  end

  def self.sum_reducer(ary)
    sum = 0
    for e in ary
      sum += e
    end
    sum
  end

  def self.factorial_reducer(ary)
    fact = 1
    for e in ary
      fact *= e
    end
    fact
  end
end
