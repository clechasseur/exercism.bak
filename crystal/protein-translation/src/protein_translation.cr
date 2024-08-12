module ProteinTranslation
  PROTEINS = {
    "Methionine"    => %w[AUG],
    "Phenylalanine" => %w[UUU UUC],
    "Leucine"       => %w[UUA UUG],
    "Serine"        => %w[UCU UCC UCA UCG],
    "Tyrosine"      => %w[UAU UAC],
    "Cysteine"      => %w[UGU UGC],
    "Tryptophan"    => %w[UGG],
    "STOP"          => %w[UAA UAG UGA],
}.flat_map do |(protein, codons)|
  codons.map { |codon| {codon, protein} }
end.to_h

  def self.proteins(strand : String) : Array(String)
    strand
      .chars
      .each_slice(3)
      .map { |nucleotides| PROTEINS[nucleotides.join]? }
      .take_while { |protein| protein != "STOP" }
      .map { |protein| protein || raise ArgumentError.new }
      .to_a
  end
end
