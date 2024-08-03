class InvalidCodonError < StandardError; end

module Translation
  CODONS = [
    [%w[AUG], "Methionine"],
    [%w[UUU UUC], "Phenylalanine"],
    [%w[UUA UUG], "Leucine"],
    [%w[UCU UCC UCA UCG], "Serine"],
    [%w[UAU UAC], "Tyrosine"],
    [%w[UGU UGC], "Cysteine"],
    [%w[UGG], "Tryptophan"],
    [%w[UAA UAG UGA], :stop]
  ].flat_map do |codons, protein|
    codons.map { |c| [c, protein] }
  end.to_h

  def self.of_rna(strand)
    strand.chars.each_slice(3).map do |nucleotides|
      CODONS[nucleotides.join]
    end.take_while do |protein|
      protein != :stop
    end.tap do |proteins|
      raise InvalidCodonError unless proteins.none?(&:nil?)
    end
  end
end
