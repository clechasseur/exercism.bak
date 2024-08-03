module Luhn
  def self.valid?(input)
    input = input.gsub(" ", "")
    return false unless input.match? /^[0-9]{2,}$/
    
    input.reverse.chars.map.with_index do |c, i|
      ci = c.to_i
      i.odd? ? (ci <= 4 ? ci * 2 : ci * 2 - 9) : ci
    end.sum % 10 == 0
  end
end
