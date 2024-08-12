def pad_left_0:
  if length % 2 == 0 then . else "0" + . end
;

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
  | [scan("\\d\\d")]
  | [.[] | to_digits | [first, (.[1] | double_digit)]]
  | flatten
  | add
  | . % 10 == 0
;

gsub("\\s+"; "")
| test("^\\d{2,}$") and (pad_left_0 | valid_luhn)
