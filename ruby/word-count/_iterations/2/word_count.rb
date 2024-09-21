class Phrase
  attr_reader :word_count

  def initialize(phrase)
    @word_count = phrase.scan(/\b[\w']+\b/).group_by(&:downcase).transform_values(&:length)
  end
end
