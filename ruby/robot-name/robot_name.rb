class Robot
  attr_reader :name

  def self.forget
  end

  def initialize
    reset
  end

  def reset
    @name = Robot::new_name
  end

  private

  @next_name = "AA000"

  def self.new_name
    name = @next_name.dup
    @next_name = "AA000" if @next_name.next!.length > 5
    name
  end
end
