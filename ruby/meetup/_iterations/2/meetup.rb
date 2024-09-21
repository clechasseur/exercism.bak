require 'date'

class Meetup
  NTHS = { :first => 0, :second => 1, :third => 2, :fourth => 3, :last => -1 }

  attr_reader :month, :year

  def initialize(month, year)
    @month = month
    @year = year
  end

  def day(weekday, nth)
    weekdays = (Date.new(year, month, 1)..Date.new(year, month, -1)).filter(&:"#{weekday}?")
    return weekdays[NTHS[nth]] unless nth == :teenth
    weekdays.find { |d| (13..19) === d.day }
  end
end
