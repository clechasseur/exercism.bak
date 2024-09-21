class Crypto
  attr_reader :plaintext

  def initialize(plaintext)
    @plaintext = plaintext
  end

  def ciphertext
    normalized = plaintext.downcase.gsub(/[^a-z0-9]+/, "")
    return "" if normalized.empty?
    c = Math.sqrt(normalized.length).ceil
    square = normalized.chars.each_slice(c).map { |r| r.join.ljust(c) }
    (0...c).map do |x|
      square.map { |r| r[x] }.join
    end.join(" ")
  end
end
