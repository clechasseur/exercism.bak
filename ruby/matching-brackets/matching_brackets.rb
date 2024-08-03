module Brackets
  NOT_BRACKET = /[^\[\]\{\}\(\)]/
  BRACKET_PAIR = /\[\]|\{\}|\(\)/

  def self.paired?(input)
    brackets = input.gsub(NOT_BRACKET, "")
    brackets.gsub!(BRACKET_PAIR, "") while BRACKET_PAIR =~ brackets
    brackets.empty?
  end
end
