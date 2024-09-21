require 'set'

class Robot
  attr_reader :name

  def self.forget
    NAMES.clear
  end

  def initialize
    reset
  end

  def reset
    @name = Robot::new_name
  end

  private

  NAMES = Set.new

  def self.new_name
    name = random_name
    name = random_name until NAMES.add? name
    name
  end

  def self.random_name
    "#{random_char}#{random_char}#{rand(1000).to_s.rjust(3, '0')}"
  end

  def self.random_char
    ("A".ord + rand(26)).chr
  end
end
