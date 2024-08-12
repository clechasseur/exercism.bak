class Scale
  SCALES = {
    :sharps => %w[A A# B C C# D D# E F F# G G#],
    :flats => %w[A Bb B C Db D Eb E F Gb G Ab]
  }

  SCALE_FOR_TONIC = {
    %w[C a G D A E B F# e b f# c# g# d#] => :sharps,
    %w[F Bb Eb Ab Db Gb d g c f bb eb] => :flats
  }.flat_map do |tonics, scale|
    tonics.map { |tonic| [tonic, scale] }
  end.to_h

  INTERVALS = {
    "m" => 1,
    "M" => 2,
    "A" => 3
  }

  attr_reader :tonic, :scale

  def initialize(tonic)
    @tonic = tonic
    @scale = SCALES[SCALE_FOR_TONIC[@tonic]]
  end

  def chromatic
    scale.rotate(scale.index(tonic.capitalize))
  end

  def interval(intervals)
    chromatic.values_at(*intervals.chars.reduce([0]) do |indexes, int|
      indexes << (indexes.last + INTERVALS[int]) % scale.size
    end)
  end
end
