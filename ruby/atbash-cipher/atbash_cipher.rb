module Atbash
  FORWARD = ('a'..'z').to_a.join
  BACKWARD = FORWARD.reverse

  def self.encode(plaintext)
    plaintext.downcase.gsub(/[^a-z0-9]+/, "").tr(FORWARD, BACKWARD).scan(/.{1,5}/).join(" ")
  end

  def self.decode(ciphertext)
    ciphertext.gsub(/\s+/, "").tr(BACKWARD, FORWARD)
  end
end
