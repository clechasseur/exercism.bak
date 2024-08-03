class AllYourBase {
  static rebase(inputBase, digits, outputBase) {
    return rebaseFromDecimal(rebaseToDecimal(inputBase, digits), outputBase)
  }

  static rebaseToDecimal(inputBase, digits) {
    if (inputBase < 2) {
      Fiber.abort("input base must be >= 2")
    }

    return digits.reduce(0) {|acc, d|
      if (d < 0 || d >= inputBase) {
        Fiber.abort("all digits must satisfy 0 <= d < input base")
      }

      return (acc * inputBase) + d
    }
  }

  static rebaseFromDecimal(decimal, outputBase) {
    if (outputBase < 2) {
      Fiber.abort("output base must be >= 2")
    }

    var outputDigits = []

    while (decimal > 0) {
      outputDigits.insert(0, decimal % outputBase)
      decimal = (decimal / outputBase).floor
    }

    if (outputDigits.count == 0) {
      outputDigits.add(0)
    }
    return outputDigits
  }
}
