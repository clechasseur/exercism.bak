object Series:
  def largestProduct(span: Int, digits: String): Option[Int] = span match
    case 0 => Some(0)
    case _ if span < 0 => None
    case _ if span > digits.length => None
    case _ if digits.exists(c => !c.isDigit) => None
    case _ => Some(digits
      .sliding(span)
      .map(series => series.map(c => c.asDigit).product)
      .max)
