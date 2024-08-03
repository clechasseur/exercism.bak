module PhoneNumber
  def self.clean(num)
    num.delete("^0-9").delete_prefix("1")[/^[2-9]\d{2}[2-9]\d{6}$/]
  end
end
