class School
  def initialize
    @grades = Hash.new { |h, g| h[g] = [] }
  end

  def grade(g)
    @grades[g].sort
  end

  def roster
    @grades.keys.sort.flat_map { |g| grade(g) }
  end

  def add(name, grade)
    return false if @grades.any? { |_, names| names.include? name }
    @grades[grade] << name
    true
  end
end
