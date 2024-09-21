module PhoneNumber
  def self.clean(num)
    /^1?(?<nanp>[2-9]\d{2}[2-9]\d{6})$/ =~ num.delete("^0-9")
    nanp
  end
end
