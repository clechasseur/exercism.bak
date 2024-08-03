module Acronym
  def self.abbreviate(phrase)
    phrase.scan(/\w+/).map { |w| w[0] }.join.upcase
  end
end
