module ResistorColor
  COLORS = %w[black brown red orange yellow green blue violet grey white]

  def self.color_code(band)
    COLORS.index(band.downcase)
  end
end
