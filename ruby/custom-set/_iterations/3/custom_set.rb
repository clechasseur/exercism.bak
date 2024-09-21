class CustomSet
  include Enumerable

  attr_reader :a

  def initialize(enum = [], sorted: false)
    @a = [*enum]
    @a.sort! unless sorted
  end

  def ==(s2)
    a == s2.a
  end

  def ===(e)
    member? e
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
    i = a.bsearch_index { _1 >= e }
    if i.nil? || a[i] != e
      a.insert(i || -1, e)
    end
    self
  end

  def difference(s2)
    self - s2
  end

  def disjoint?(s2)
    (self & s2).empty?
  end

  def each(&block)
    return to_enum(:each) { size } unless block_given?
    a.each(&block)
    self
  end

  def empty?
    a.empty?
  end

  def intersection(s2)
    self & s2
  end

  def member?(e)
    found = a.bsearch { _1 >= e }
    !found.nil? && found == e
  end

  def size
    a.size
  end

  def subset?(s2)
    (self - s2).empty?
  end

  def union(s2)
    self | s2
  end

  alias_method :eql?, :==
  alias_method :include?, :member?

  private

  def iterate_and_merge(s2, append_s1: false, append_s2: false)
    r = []
    i1 = i2 = 0
    while i1 < size && i2 < s2.size
      cmp = a[i1] <=> s2.a[i2]
      yield r, cmp, a[i1], s2.a[i2]
      i1 += 1 if cmp <= 0
      i2 += 1 if cmp >= 0
    end
    if append_s1 && i1 < size
      r.push(*a[i1..-1])
    elsif append_s2 && i2 < s2.size
      r.push(*s2.a[i2..-1])
    end
    CustomSet.new(r, sorted: true)
  end
end
