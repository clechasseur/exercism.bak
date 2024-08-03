module IsbnVerifier
  def self.valid?(isbn)
    /^\d{9}[\dX]$/ =~ isbn.gsub(/-/, "") && $&.chars.map.with_index do |d, i|
      (d == "X" ? 10 : d.to_i) * (10 - i)
    end.sum % 11 == 0
  end
end
