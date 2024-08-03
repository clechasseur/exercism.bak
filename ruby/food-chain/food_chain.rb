module FoodChain
  ChainInfo = Struct.new(:name, :line, :addend)

  CHAIN_INFOS = [
    ChainInfo.new("fly", nil, ""),
    ChainInfo.new("spider", "It wriggled and jiggled and tickled inside her.", " that wriggled and jiggled and tickled inside her"),
    ChainInfo.new("bird", "How absurd to swallow a bird!", ""),
    ChainInfo.new("cat", "Imagine that, to swallow a cat!", ""),
    ChainInfo.new("dog", "What a hog, to swallow a dog!", ""),
    ChainInfo.new("goat", "Just opened her throat and swallowed a goat!", ""),
    ChainInfo.new("cow", "I don't know how she swallowed a cow!", "")
  ]
  LAST_VERSE = "I know an old lady who swallowed a horse.\n" +
    "She's dead, of course!\n"

  def self.song
    (0..7).map { |n| verse n }.join("\n")
  end

  def self.verse(n)
    return LAST_VERSE if n == 7

    ci = CHAIN_INFOS[n]
    v = "I know an old lady who swallowed a #{ci.name}.\n"
    v << "#{ci.line}\n" unless ci.line.nil?
    v << (n - 1).downto(0).map do |nprime|
      ciprime = CHAIN_INFOS[nprime]
      "She swallowed the #{CHAIN_INFOS[nprime + 1].name} to catch the #{ciprime.name}#{ciprime.addend}.\n"
    end.join
    v << "I don't know why she swallowed the #{CHAIN_INFOS[0].name}. Perhaps she'll die.\n"
    v
  end
end
