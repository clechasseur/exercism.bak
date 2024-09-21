class Library
  def self.first_letter(title : String) : Char
    title[0]
  end

  def self.initials(first_name : String, last_name : String) : String
    "#{first_letter(first_name)}.#{first_letter(last_name)}"
  end

  def self.decrypt_character(character : Char) : Char
    is_upper = character.uppercase?
    character = 'a' + (character.downcase.ord - 'a'.ord - 1) % 26
    character = character.upcase if is_upper
    character
  end

  def self.decrypt_text(text : String) : String
    text.chars.map { |c| c.letter? ? decrypt_character(c) : c }.join
  end
end
