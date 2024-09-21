module Complement
  NUCLEOTIDES = {
    "G" => "C",
    "C" => "G",
    "T" => "A",
    "A" => "U"
  }

  def self.of_dna(dna)
    dna.chars.map { |nucleotide| NUCLEOTIDES[nucleotide] }.join
  end
end
