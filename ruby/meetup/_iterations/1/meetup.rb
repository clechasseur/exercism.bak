require 'date'

class Date
  def next_weekday(weekday)
    d = self
    d = d.next_day until d.__send__("#{weekday.to_s}?")
    d
  end
end

class Meetup
  attr_reader :month, :year

  def initialize(month, year)
    @month = month
    @year = year
  end

  def day(weekday, nth)
    weekdays = all_weekdays(weekday)
    return weekdays[NTHS[nth]] unless nth == :teenth
    weekdays.find { |d| (13..19) === d.day }
  end

  private

  NTHS = { :first => 0, :second => 1, :third => 2, :fourth => 3, :last => -1 }

  def all_weekdays(weekday)
    weekdays = []
    d = Date.new(year, month, 1).next_weekday(weekday)
    while d.month == month
      weekdays << d
      d = d + 7
    end
    weekdays
  end
end
