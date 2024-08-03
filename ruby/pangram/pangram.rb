module Pangram
  def self.pangram?(phrase)
    phrase.downcase.delete("^a-z").chars.uniq.size == 26
  end
end
