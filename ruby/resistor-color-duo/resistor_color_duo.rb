module ResistorColorDuo
  COLORS = %w[black brown red orange yellow green blue violet grey white]

  def self.single_value(color)
    COLORS.find_index(color)
  end
  
  def self.value(colors)
    single_value(colors[0]) * 10 + single_value(colors[1])
  end
end
