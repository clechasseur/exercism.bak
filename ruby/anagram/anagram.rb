class Anagram
  attr_reader :target, :target_chars

  def initialize(target)
    @target = target.downcase
    @target_chars = @target.chars.sort
  end

  def match(candidates)
    candidates.filter do |candidate|
      cd = candidate.downcase
      cd != target && cd.chars.sort == target_chars
    end
  end
end
