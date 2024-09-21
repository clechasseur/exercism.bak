class Phrase
  attr_reader :word_count

  def initialize(phrase)
    @word_count = phrase.scan(/\w+(?:'\w+)?/).reduce(Hash.new(0)) do |hash, word|
      hash[word.downcase] = hash[word.downcase] + 1
      hash
    end
  end
end
