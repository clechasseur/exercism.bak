class Crypto
  attr_reader :plaintext

  def initialize(plaintext)
    @plaintext = plaintext
  end

  def ciphertext
    normalized = plaintext.downcase.gsub(/[^a-z0-9]+/, "")
    return "" if normalized.empty?
    c = Math.sqrt(normalized.length).ceil
    normalized.chars.each_slice(c).map do |r|
      r + [' '] * (c - r.size)
    end.transpose.map(&:join).join(" ")
  end
end
