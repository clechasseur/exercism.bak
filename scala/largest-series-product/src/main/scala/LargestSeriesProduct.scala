object Series:
  def largestProduct(span: Int, digits: String): Option[Int] =
    if span < 0 || span > digits.length || digits.exists(!_.isDigit) then None
    else if span == 0 then Some(1)
    else Some(digits.sliding(span).map(_.map(_.asDigit).product).max)
