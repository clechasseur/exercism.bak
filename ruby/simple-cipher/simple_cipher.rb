class String
  def letter_ord
    ord - "a".ord
  end
end

class Integer
  def letter_chr
    (self % 26 + "a".ord).chr
  end
end

class Cipher
  attr_reader :key

  def initialize(key = Cipher::random_key)
    raise ArgumentError, "Invalid key: #{key}" unless /^[a-z]+$/ =~ key
    @key = key
  end

  def encode(plaintext)
    encode_decode(plaintext, :+)
  end

  def decode(ciphertext)
    encode_decode(ciphertext, :-)
  end

  private

  def encode_decode(text, op)
    text.chars.zip(key.chars.cycle).map do |c, k|
      (c.letter_ord.__send__(op, k.letter_ord)).letter_chr
    end.join
  end

  def self.random_key
    100.times.map { ("a".ord + rand(26)).chr }.join
  end  
end
