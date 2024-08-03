module RunLengthEncoding
  def self.encode(input)
    input.gsub(/(.)\1+/) { "#{$&.length}#{$1}" }
  end

  def self.decode(output)
    output.gsub(/(\d+)(.)/) { $2 * $1.to_i }
  end
end
