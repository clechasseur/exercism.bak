module Complement
  DNA = "GCTA"
  RNA = "CGAU"

  def self.of_dna(dna)
    dna.tr(DNA, RNA)
  end
end
