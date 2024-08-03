class ResistorColorTrio
  def initialize(bands)
    @ohms = (value(bands[0]) * 10 + value(bands[1])) * (10 ** value(bands[2]))
  end

  def label
    ohms = @ohms
    kilo = ohms >= 1000
    ohms /= 1000 if kilo
    "Resistor value: #{ohms} #{kilo ? 'kilo' : ''}ohms"
  end

  private

  COLORS = %w[black brown red orange yellow green blue violet grey white]

  def value(band)
    val = COLORS.find_index(band)
    raise ArgumentError, "Wrong band color: #{band}" if val.nil?
    val
  end
end
