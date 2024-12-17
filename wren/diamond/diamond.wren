class Diamond {
  static rows(letter) {
    if (letter == "A") {
      return ["A"]
    }

    var letterOrd = ord(letter)
    var lineSize = (letterOrd - aOrd) * 2 + 1
    return ((aOrd..letterOrd).toList + ((letterOrd - 1)..aOrd).toList).
      map { |l| line(chr(l), lineSize) }.
      toList
  }

  static line(letter, lineSize) {
    if (letter == "A") {
      var outerWhitespace = " " * ((lineSize - 1) / 2)
      return "%(outerWhitespace)%(letter)%(outerWhitespace)"
    }

    var innerSize = (ord(letter) - aOrd) * 2 - 1
    var outerSize = (lineSize - innerSize - 2) / 2
    var innerWhitespace = " " * innerSize
    var outerWhitespace = " " * outerSize
    return "%(outerWhitespace)%(letter)%(innerWhitespace)%(letter)%(outerWhitespace)"
  }

  static ord(letter) {
    return letter.bytes[0]
  }

  static aOrd {
    __aOrd = __aOrd || ord("A")
    return __aOrd
  }

  static chr(b) {
    return String.fromByte(b)
  }
}
