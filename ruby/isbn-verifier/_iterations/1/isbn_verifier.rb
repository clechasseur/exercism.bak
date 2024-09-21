module IsbnVerifier
  def self.valid?(isbn)
    /^\d{9}[\dX]$/ =~ isbn.gsub(/-/, "") && $&.chars.reverse.map.with_index do |d, i|
      (d == "X" ? 10 : d.to_i) * (i + 1)
    end.reduce(&:+) % 11 == 0
  end
end
