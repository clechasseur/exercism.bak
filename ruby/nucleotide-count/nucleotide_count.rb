class Nucleotide
  EMPTY_HISTOGRAM = "ACGT".chars.product([0]).to_h

  attr_reader :histogram

  def self.from_dna(seq)
    raise ArgumentError, "Invalid DNA sequence: #{seq}" unless /^[ACGT]*$/ =~ seq
    
    histogram = seq.chars.sort.chunk_while { |a, b| a == b }.map do |nucleotides|
      [nucleotides.first, nucleotides.size]
    end.to_h
    new(EMPTY_HISTOGRAM.merge(histogram))
  end

  def initialize(histogram)
    @histogram = histogram
  end

  def count(nucleotide)
    histogram[nucleotide]
  end
end
