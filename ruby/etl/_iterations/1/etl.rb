module ETL
  def self.transform(old)
    old.to_a.flat_map do |(score, letters)|
      letters.map { |l| [l.downcase, score] }
    end.to_h
  end
end
