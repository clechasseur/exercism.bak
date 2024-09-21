module ETL
  def self.transform(old)
    old.flat_map { |(score, letters)| letters.map(&:downcase).product([score]) }.to_h
  end
end
