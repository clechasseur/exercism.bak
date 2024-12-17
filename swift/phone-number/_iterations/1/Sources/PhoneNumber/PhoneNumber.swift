class PhoneNumber {
  let originalNumber: String

  init(_ number: String) {
    self.originalNumber = number
  }

  func clean() throws -> String {
    let digitsOnly = self.originalNumber.replacing(nonDigitRegex, with: "")
    if let cleanedMatch = digitsOnly.wholeMatch(of: phoneNumberRegex) {
      return String(cleanedMatch.1)
    }

    throw PhoneNumberError.invalidPhoneNumber
  }
}

let nonDigitRegex = #/\D+/#
let phoneNumberRegex = #/1?([2-9]\d{2}[2-9]\d{2}\d{4})/#

enum PhoneNumberError: Error {
  case invalidPhoneNumber
}
