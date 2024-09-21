module Bob
  def self.hey(remark)
    return "Fine. Be that way!" if silent?(remark)
    
    case [question?(remark), yelling?(remark)]
      when [true, true]
        "Calm down, I know what I'm doing!"
      when [false, true]
        "Whoa, chill out!"
      when [true, false]
        "Sure."
      else
        "Whatever."
    end
  end

  def self.silent?(remark)
    !!(/\A\s*\Z/ =~ remark)
  end

  def self.question?(remark)
    !!(/\?\s*\Z/ =~ remark)
  end

  def self.yelling?(remark)
    !!(/\A[^a-z]*[A-Z][^a-z]*\Z/ =~ remark)
  end
end
