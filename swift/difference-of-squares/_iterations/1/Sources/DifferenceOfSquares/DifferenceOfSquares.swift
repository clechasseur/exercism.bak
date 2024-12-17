import Foundation

class Squares {
  let n: Int

  init(_ n: Int) {
    self.n = n
  }

  lazy var squareOfSum = (1...n).reduce(0, +) ^^ 2
  lazy var sumOfSquares = (1...n).map { $0 ^^ 2 }.reduce(0, +)
  lazy var differenceOfSquares = squareOfSum - sumOfSquares
}

precedencegroup PowerPrecedence { higherThan: MultiplicationPrecedence }
infix operator ^^ : PowerPrecedence
func ^^ (radix: Int, power: Int) -> Int {
    return Int(pow(Double(radix), Double(power)))
}
