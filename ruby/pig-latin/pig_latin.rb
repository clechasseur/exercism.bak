module PigLatin
  def self.translate(phrase)
    phrase.gsub(/\w+/) { translate_word(_1) }
  end

  def self.translate_word(word)
    case word
    when /^(?:[aeiou]|xr|yt)/
      "#{word}ay"
    when /^([^aeiouq]*qu|[^aeiou][^aeiouy]*)(.+)$/
      "#{$2}#{$1}ay"
    else
      raise RuntimeError, "Cannot translate #{word}"
    end
  end
end
