class Affine
  M = "z".ord - "a".ord + 1

  attr_reader :a, :b, :mmi

  def initialize(a, b)
    @a = a
    @b = b
    @mmi = calculate_mmi
  end

  def encode(plaintext)
    plaintext.downcase.gsub(/[^a-z0-9]+/, "").gsub(/[a-z]/) do |c|
      encode_one(c)
    end.gsub(/.{1,5}/, "\\0 ").rstrip
  end

  def decode(ciphertext)
    ciphertext.downcase.gsub(/\s+/, "").gsub(/[a-z]/) do |c|
      decode_one(c)
    end
  end

  private

  def encode_one(c)
    # Encryption function: E(x) = (ai + b) mod m
    i = c.ord - "a".ord
    ex = (a * i + b) % M
    (ex + "a".ord).chr
  end

  def decode_one(c)
    # Decryption function: D(y) = (a^-1)(y - b) mod m
    y = c.ord - "a".ord
    dy = (mmi * (y - b)) % M
    (dy + "a".ord).chr
  end

  def calculate_mmi
    # Taken from https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
    # Ported from my Java solution: https://exercism.org/tracks/java/exercises/affine-cipher/solutions/clechasseur
    t = 0
    new_t = 1
    r = M
    new_r = a
    while new_r.nonzero?
      quotient = r / new_r
      old_t = t
      t = new_t
      new_t = old_t - quotient * new_t
      old_r = r
      r = new_r
      new_r = old_r - quotient * new_r
    end
    raise ArgumentError, "Key a (#{a}) and alphabet size (#{M}) must be coprime" if r > 1
    t % M
  end
end