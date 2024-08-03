module FlattenArray
  # Cheat:
  #def self.flatten(ary)
  #  ary.flatten.compact
  #end

  # The hard way:
  def self.flatten(o)
    return [] if o.nil?
    return [o] unless o.is_a? Array
    o.map { |e| flatten(e) }.reduce([], &:+)
  end
end
