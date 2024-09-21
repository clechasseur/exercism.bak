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

  attr_reader :tonic, :scale, :tonic_index

  def initialize(tonic)
    @tonic = tonic
    @scale = SCALES[SCALE_FOR_TONIC[@tonic]]
    @tonic_index = @scale.index("#{@tonic[0].upcase}#{@tonic[1..]}")
  end

  def chromatic
    scale.rotate(tonic_index)
  end

  def interval(intervals)
    intervals.chars.reduce([tonic_index]) do |indexes, int|
      indexes << (indexes.last + INTERVALS[int]) % scale.size
    end.map do |index|
      scale[index]
    end
  end
end
