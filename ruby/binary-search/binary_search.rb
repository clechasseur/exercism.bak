class BinarySearch
  attr_reader :arr

  def initialize(arr)
    @arr = arr
  end

  def search_for(e)
    search_for_in(e, 0..(arr.size - 1))
  end

  private

  def search_for_in(e, range)
    return nil if range.size.zero?
    middle = range.begin + range.size / 2
    case e <=> arr[middle]
    when 0
      middle
    when -1
      search_for_in(e, range.begin..(middle - 1))
    when 1
      search_for_in(e, (middle + 1)..range.end)
    end
  end
end
