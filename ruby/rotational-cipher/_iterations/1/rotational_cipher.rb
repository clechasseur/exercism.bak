module RotationalCipher
  ALPHABET = ('a'..'z').to_a.join
  ALPHABET_TWICE = ALPHABET * 2

  def self.rotate(text, key)
    rep = ALPHABET_TWICE[key, ALPHABET.length]
    text.gsub(/[a-zA-Z]+/) { |w| w.tr(ALPHABET + ALPHABET.upcase, rep + rep.upcase) }
  end
end
