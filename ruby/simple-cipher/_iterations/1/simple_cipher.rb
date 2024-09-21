class Cipher
  ALPHABET = ('a'..'z').to_a.join
  ALPHABET_TWICE = ALPHABET * 2

  attr_reader :key

  def initialize(key = Cipher::random_key)
    raise ArgumentError, "Invalid key: #{key}" unless /^[a-z]+$/ =~ key
    @key = key
  end

  def encode(plaintext)
    plaintext.chars.zip(key.chars.cycle.take(plaintext.length)).map do |c, k|
      c.tr(ALPHABET, ALPHABET_TWICE[ALPHABET.index(k), 26])
    end.join
  end

  def decode(ciphertext)
    ciphertext.chars.zip(key.chars.cycle.take(ciphertext.length)).map do |c, k|
      c.tr(ALPHABET_TWICE[ALPHABET.index(k), 26], ALPHABET)
    end.join
  end

  private

  def self.random_key
    Array.new(100) { (97 + rand(26)).chr }.join
  end  
end
