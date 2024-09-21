module Raindrops
  SOUNDS = [[3, "Pling"], [5, "Plang"], [7, "Plong"]]

  def self.convert(n)
    sounds = SOUNDS.reduce("") { |s, sound| n % sound[0] == 0 ? s + sound[1] : s }
    sounds.empty? ? n.to_s : sounds
  end
end
