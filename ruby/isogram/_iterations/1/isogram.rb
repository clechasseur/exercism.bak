module Isogram
  def self.isogram?(input)
    !input.downcase.match? /(\w).*\1/
  end
end
