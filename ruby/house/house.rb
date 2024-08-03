module House
  LINES = [
    ["malt", "lay in"],
    ["rat", "ate"],
    ["cat", "killed"],
    ["dog", "worried"],
    ["cow with the crumpled horn", "tossed"],
    ["maiden all forlorn", "milked"],
    ["man all tattered and torn", "kissed"],
    ["priest all shaven and shorn", "married"],
    ["rooster that crowed in the morn", "woke"],
    ["farmer sowing his corn", "kept"],
    ["horse and the hound and the horn", "belonged to"]
  ]

  def self.recite
    (-1...LINES.size).map { verse _1 }.join("\n")
  end

  def self.verse(n)
    lines = n.downto(0).map { line _1 }.join
    "This is #{lines}the house that Jack built.\n"
  end

  def self.line(n)
    "the #{LINES[n][0]}\nthat #{LINES[n][1]} "
  end
end
