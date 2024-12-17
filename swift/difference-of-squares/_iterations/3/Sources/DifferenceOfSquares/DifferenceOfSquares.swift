import Foundation

class Squares {
  let n: Int

  init(_ n: Int) {
    self.n = n
  }

  lazy var squareOfSum = (1...n).reduce(0, +).squared
  lazy var sumOfSquares = (1...n).map { $0.squared }.reduce(0, +)
  lazy var differenceOfSquares = squareOfSum - sumOfSquares
}

extension Int {
  var squared: Int {
    get {
      return self * self
    }
  }
}
