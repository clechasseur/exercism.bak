class Microwave
  attr_reader :timer

  def initialize(input)
    minutes, seconds = input.divmod(100)
    if seconds >= 60
      minutes += 1
      seconds -= 60
    end
    @timer = "#{minutes.to_s.rjust(2, '0')}:#{seconds.to_s.rjust(2, '0')}"
  end
end
