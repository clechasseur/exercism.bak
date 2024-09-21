module Port
  def self.get_identifier(city)
    city.upcase[0..3].to_sym
  end

  def self.get_terminal(ship_identifier)
    TERMINALS_BY_CARGO[cargo(ship_identifier)]
  end

  def self.cargo(ship_identifier)
    ship_identifier.to_s[0..2].upcase
  end

  TERMINALS_BY_CARGO = Hash.new(:B).tap do |h|
    h.merge!(['OIL', 'GAS'].map { |cargo| [cargo, :A] }.to_h)
  end.freeze

  IDENTIFIER = get_identifier("Palermo")
end
