class WordProblem
  OPS = {
    "plus" => :+,
    "minus" => :-,
    "multiplied by" => :*,
    "divided by" => :/
  }
  ANY_OP = OPS.keys.join('|')
  PHRASE_PATTERN = Regexp.new("^What is (?<memo>-?\\d+)(?<ops>(?: (?:#{ANY_OP}) -?\\d+)*)\\?$")
  OPS_PATTERN = Regexp.new(" (#{ANY_OP}) (-?\\d+)")

  def initialize(phrase)
    @phrase = phrase
  end

  def answer
    match = PHRASE_PATTERN.match(@phrase)
    raise ArgumentError, "Invalid problem phrase: #{@phrase}" if match.nil?
    value = match[:memo].to_i
    match[:ops].scan(OPS_PATTERN) do |op, new_value|
      value = value.__send__(OPS[op], new_value.to_i)
    end
    value
  end
end
