module ReverseString
  def self.reverse(value : String) : String
    String.build do |str|
      i = -1
      while i >= -value.size
        str << value[i]
        i -= 1
      end
    end
  end
end
