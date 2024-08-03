class School
  def initialize
    @grades = Hash.new { |h, g| h[g] = [] }
  end

  def grade(g)
    @grades[g].sort
  end

  def roster
    @grades.to_a.sort_by(&:first).map(&:last).flat_map(&:sort)
  end

  def add(name, grade)
    return false if @grades.any? { |_, names| names.include? name }
    @grades[grade] << name
    true
  end
end
