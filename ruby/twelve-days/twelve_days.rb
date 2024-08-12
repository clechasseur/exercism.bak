module TwelveDays
  LYRICS_INFO = [
    {nth: "first", gift: "a Partridge in a Pear Tree"},
    {nth: "second", gift: "two Turtle Doves"},
    {nth: "third", gift: "three French Hens"},
    {nth: "fourth", gift: "four Calling Birds"},
    {nth: "fifth", gift: "five Gold Rings"},
    {nth: "sixth", gift: "six Geese-a-Laying"},
    {nth: "seventh", gift: "seven Swans-a-Swimming"},
    {nth: "eighth", gift: "eight Maids-a-Milking"},
    {nth: "ninth", gift: "nine Ladies Dancing"},
    {nth: "tenth", gift: "ten Lords-a-Leaping"},
    {nth: "eleventh", gift: "eleven Pipers Piping"},
    {nth: "twelfth", gift: "twelve Drummers Drumming"}
  ]

  def self.gift(day)
    case day
    when 1
      LYRICS_INFO[0][:gift]
    when 2
      LYRICS_INFO[1][:gift] + ", and " + gift(1)
    else
      LYRICS_INFO[day - 1][:gift] + ", " + gift(day - 1)
    end
  end

  def self.lyric(day)
    "On the #{LYRICS_INFO[day - 1][:nth]} day of Christmas my true love gave to me: #{gift(day)}.\n"
  end

  def self.lyrics(days)
    days.map { |day| lyric(day) }.join("\n")
  end

  def self.song
    lyrics(1..LYRICS_INFO.length)
  end
end
