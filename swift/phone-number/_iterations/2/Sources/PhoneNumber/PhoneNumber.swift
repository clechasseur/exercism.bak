class PhoneNumber {
  let originalNumber: String

  init(_ number: String) {
    self.originalNumber = number
  }

  func clean() throws -> String {
    let digitsOnly = self.originalNumber.filter { $0.isNumber }
    if let cleanedMatch = digitsOnly.wholeMatch(of: phoneNumberRegex) {
      return String(cleanedMatch.1)
    }

    throw PhoneNumberError.invalidPhoneNumber
  }
}

let phoneNumberRegex = #/1?([2-9]\d{2}[2-9]\d{2}\d{4})/#

enum PhoneNumberError: Error {
  case invalidPhoneNumber
}
