module ReverseString
  def self.reverse(value : String) : String
    String.build do |str|
      ReverseStringIterator.new(value).each do |c|
        str << c
      end
    end
  end

  private class ReverseStringIterator
    include Iterator(Char)

    def initialize(@source : String)
      @idx = -1
    end

    def next
      return stop if @idx < -@source.size

      c = @source[@idx]
      @idx -= 1
      c
    end
  end
end
