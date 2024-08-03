module Raindrops
  SOUNDS = [[3, "Pling"], [5, "Plang"], [7, "Plong"]]

  def self.convert(n)
    sounds = SOUNDS.reduce("") { |s, (factor, sound)| n % factor == 0 ? s + sound : s }
    sounds.empty? ? n.to_s : sounds
  end
end
