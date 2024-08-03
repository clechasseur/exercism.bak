class PasswordLock
  def initialize(password : Int32 | String | Float64)
    @password = password
  end

  def encrypt
    @password = PasswordLock.encrypted(@password)
  end

  def unlock?(guess)
    @password == PasswordLock.encrypted(guess) ? "Unlocked" : nil
  end

  def self.encrypted(password)
    if password.is_a?(Int32)
      (password / 2).round
    elsif password.is_a?(String)
      password.reverse
    else
      password * 4
    end
  end
end
