module Bob
  def self.hey(remark)
    return "Fine. Be that way!" if /\A\s*\Z/ =~ remark
    question = /\?\s*\Z/ =~ remark
    yelling = /\A[^a-z]*[A-Z][^a-z]*\Z/ =~ remark
    return "Calm down, I know what I'm doing!" if question && yelling
    return "Whoa, chill out!" if yelling
    return "Sure." if question
    "Whatever."
  end
end
