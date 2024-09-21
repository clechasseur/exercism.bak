def reverse_string:
  explode | reverse | implode
;

def to_digits:
  ("0" | explode | first) as $zero
  | explode
  | [.[] - $zero]
;

def double_digit:
  . * 2
  | if . > 9 then . - 9 else . end
;

def valid_luhn:
  reverse_string
  | [scan("\\d\\d?")]
  | [.[] | to_digits]
  | [.[] | [first, ((.[1]? // 0) | double_digit)]]
  | flatten
  | add
  | . % 10 == 0
;

gsub("\\s+"; "")
| test("^\\d{2,}$") and valid_luhn
