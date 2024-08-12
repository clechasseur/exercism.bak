module Isogram
  def self.isogram?(input)
    !input.match? /(\w).*\1/i
  end
end
