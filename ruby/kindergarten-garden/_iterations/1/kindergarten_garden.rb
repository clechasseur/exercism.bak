class Garden
  STUDENTS = %w[alice bob charlie david eve fred ginny harriet ileana joseph kincaid larry]
  PLANTS = { "G" => :grass, "C" => :clover, "R" => :radishes, "V" => :violets }

  def initialize(plants)
    l1, l2 = plants.lines.map { _1.chomp.chars }
    @plants = l1.zip(l2).each_slice(2).with_index.map do |((a, c), (b, d)), index|
      [a, b, c, d].map { PLANTS[_1] }
    end
  end

  STUDENTS.each do |name|
    define_method(name) do
      @plants[name[0].ord - "a".ord]
    end
  end
end
