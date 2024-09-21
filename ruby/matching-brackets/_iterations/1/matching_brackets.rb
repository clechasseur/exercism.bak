module Brackets
  def self.paired?(input)
    brackets = input.gsub(/[^\[\]\{\}\(\)]/, "")
    while !brackets.empty?
      new_brackets = brackets.gsub(/\[\]|\{\}|\(\)/, "")
      return false if new_brackets == brackets
      brackets = new_brackets
    end
    true
  end
end
