class Array
  def reverse_if(flag)
    flag ? self.reverse : self
  end
end

class SecretHandshake
  EVENTS = ["wink", "double blink", "close your eyes", "jump"]
  REVERSE_EVENT = 1 << 4

  def initialize(handshake)
    @handshake = handshake.to_i
  end

  def commands
    EVENTS.filter.with_index do |_, i|
      (@handshake & 1 << i).nonzero?
    end.reverse_if((@handshake & REVERSE_EVENT).nonzero?)
  end
end
