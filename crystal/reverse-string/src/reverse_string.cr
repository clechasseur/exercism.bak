module ReverseString
  def self.reverse(value : String) : String
    # Using `value.reverse` would be cheating here,
    # so let's try to learn about Iterators instead.

    ReverseStringIterator.new(value).join("")
  end

  private class ReverseStringIterator
    include Iterator(Char)

    def initialize(@source : String)
      @idx = -1
    end

    def next
      return stop if @idx < -@source.size

      @source[@idx].tap { @idx -= 1 }
    end
  end
end
