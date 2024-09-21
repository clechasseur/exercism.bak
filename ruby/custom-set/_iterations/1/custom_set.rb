class CustomSet
  def initialize(enum = [], sorted: false)
    @ary = [*enum]
    @ary.sort! unless sorted
  end

  def ==(s2)
    @ary == s2.to_a
  end

  def |(s2)
    iterate_and_merge(s2, append_s1: true, append_s2: true) do |r, cmp, e1, e2|
      if cmp <= 0
        r << e1
      else
        r << e2
      end
    end
  end

  def -(s2)
    iterate_and_merge(s2, append_s1: true) do |r, cmp, e1, *|
      r << e1 if cmp < 0
    end
  end

  def &(s2)
    iterate_and_merge(s2) do |r, cmp, e1, *|
      r << e1 if cmp == 0
    end
  end

  def +(s2)
    self | s2
  end

  def add(e)
    i = @ary.bsearch_index { _1 >= e }
    if i.nil? || @ary[i] != e
      @ary.insert(i || -1, e)
    end
    self
  end

  def difference(s2)
    self - s2
  end

  def disjoint?(s2)
    (self & s2).empty?
  end

  def empty?
    @ary.empty?
  end

  def intersection(s2)
    self & s2
  end

  def member?(e)
    found = @ary.bsearch { _1 >= e }
    !found.nil? && found == e
  end

  def size
    @ary.size
  end

  def subset?(s2)
    (self - s2).empty?
  end

  def to_a
    @ary
  end

  def union(s2)
    self | s2
  end

  alias_method :eql?, :==

  private

  def iterate_and_merge(s2, append_s1: false, append_s2: false)
    r = []
    ary2 = s2.to_a
    i1 = i2 = 0
    while i1 < @ary.size && i2 < ary2.size
      cmp = @ary[i1] <=> ary2[i2]
      yield r, cmp, @ary[i1], ary2[i2]
      i1 += 1 if cmp <= 0
      i2 += 1 if cmp >= 0
    end
    if append_s1 && i1 < @ary.size
      r.push(*@ary[i1..-1])
    elsif append_s2 && i2 < ary2.size
      r.push(*ary2[i2..-1])
    end
    CustomSet.new(r, sorted: true)
  end
end
