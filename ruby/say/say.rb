class Say
  def initialize(n)
    @n = n
  end

  def in_english
    raise ArgumentError, "Invalid number" unless (0...1_000_000_000_000) === @n

    return "zero" if @n.zero?

    r = @n
    chunks = []
    while r != 0
      r, n = r.divmod 1_000
      chunks << less_than_one_thousand(n)
    end
    chunks.zip(SCALES).filter { |n, *| !n.empty? }.reverse.flatten.compact.join(THOUSAND_SEPARATOR)
  end

  private

  SCALES = [nil, "thousand", "million", "billion"]
  HUNDRED = "hundred"
  TENS = %w[twenty thirty forty fifty sixty seventy eighty ninety]
  ONES = %w[one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen]
  THOUSAND_SEPARATOR = " "
  HUNDRED_SEPARATOR = " "
  TEN_SEPARATOR = "-"

  def less_than_one_thousand(n)
    h, l = n.divmod 100
    r = []
    r << less_than_one_hundred(h) << HUNDRED if h.nonzero?
    r << less_than_one_hundred(l) if l.nonzero?
    r.join(HUNDRED_SEPARATOR)
  end

  def less_than_one_hundred(n)
    return less_than_twenty(n) if n < 20
    t, l = n.divmod 10
    r = [TENS[t - 2]]
    r << less_than_twenty(l) if l.nonzero?
    r.join(TEN_SEPARATOR)
  end

  def less_than_twenty(n)
    ONES[n - 1]
  end
end
