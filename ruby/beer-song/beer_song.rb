module BeerSong
  def self.recite(start_verse, num_verses)
    start_verse.downto(start_verse - num_verses + 1).map do |i|
      "#{beers(i).capitalize} on the wall, #{beers(i)}.\n" +
      "#{action(i)}, #{beers((i + 99) % 100)} on the wall.\n"
    end.join("\n")
  end

  private

  def self.beers(i)
    "#{i.zero? ? 'no more' : i} bottle#{i == 1 ? '' : 's'} of beer"
  end

  def self.action(i)
    i.zero? ? "Go to the store and buy some more" : "Take #{i == 1 ? 'it' : 'one'} down and pass it around"
  end
end
