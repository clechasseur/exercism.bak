(module
  (memory (export "mem") 1)

  ;; This solution is essentially a WASM port of my Elm solution:
  ;; https://exercism.org/tracks/elm/exercises/run-length-encoding

  ;; Store digits in memory for easy reference
  (data (i32.const 0) "0123456789")

  ;;
  ;; Outputs a number as a string
  ;;
  ;; @param {i32} offset - The offset where to write the number in linear memory
  ;; @param {i32} input - The number to write
  ;;
  ;; @returns {i32} - The offset past the point where the number was written in linear memory
  ;;
  (func $itoa (param $offset i32) (param $input i32) (result i32)
    (local $digit i32)

    (local.set $digit
      (i32.rem_u (local.get $input) (i32.const 10)))

    (local.get $offset)
    (if (param i32) (result i32)
      (i32.ne
        (local.tee $input (i32.div_u (local.get $input) (i32.const 10)))
        (i32.const 0))
      (then
        (local.get $input)
        (call $itoa)))

    (memory.copy (local.tee $offset) (local.get $digit) (i32.const 1))
    (i32.add (local.get $offset) (i32.const 1)))

  ;;
  ;; Encode a single character using run-length encoding
  ;;
  ;; @param {i32} offset - The offset where to write the output in linear memory
  ;; @param {i32} count - The number of times the character appeared in the input
  ;; @param {i32} char - The character to encode
  ;;
  ;; @returns {i32} - The offset past the point where the character was encoded in linear memory
  ;;
  (func $encodeChar (param $offset i32) (param $count i32) (param $char i32) (result i32)
    (local.get $offset)
    (if (param i32) (result i32)
      (i32.gt_s (local.get $count) (i32.const 1))
      (then
        (local.get $count)
        (call $itoa)))

    (i32.store8 (local.tee $offset) (local.get $char))
    (i32.add (local.get $offset) (i32.const 1)))

  ;;
  ;; Recursively encodes a string using run-length encoding in linear memory
  ;;
  ;; @param {i32} outputOffset - The offset where to output the encoded string in linear memory
  ;; @param {i32} count - The number of times the previous character was seen
  ;; @param {i32} prevChar - The character seen in the previous iteration(s)
  ;; @param {i32} inputOffset - The offset containing the string to encode in linear memory
  ;; @param {i32} inputLength - The length of the input string
  ;;
  ;; @returns {i32} - The offset past the end of the encoded string in linear memory
  ;;
  (func $tailrecEncode
    (param $outputOffset i32) (param $count i32) (param $prevChar i32) (param $inputOffset i32) (param $inputLength i32)
    (result i32)

    (local $curChar i32)
    
    (if
      (i32.eqz (local.get $inputLength))
      (then
        (return
          (call $encodeChar (local.get $outputOffset) (local.get $count) (local.get $prevChar)))))

    (local.get $outputOffset)
    (local.get $count)
    (if (param i32) (param i32) (result i32 i32)
      (i32.ne
        (local.tee $curChar (i32.load8_u (local.get $inputOffset)))
        (local.get $prevChar))
      (then
        (local.get $prevChar)
        (call $encodeChar)
        (i32.const 0)))

    (i32.const 1)
    (i32.add)
    (local.get $curChar)
    (i32.add (local.get $inputOffset) (i32.const 1))
    (i32.sub (local.get $inputLength) (i32.const 1))
    (call $tailrecEncode)) ;; Could be `return_call` in the future

  ;;
  ;; Encode a string using run-length encoding
  ;;
  ;; @param {i32} inputOffset - The offset of the input string in linear memory
  ;; @param {i32} inputLength - The length of the input string in linear memory
  ;;
  ;; @returns {(i32,i32)} - The offset and length of the encoded string in linear memory
  ;;
  (func (export "encode") (param $inputOffset i32) (param $inputLength i32) (result i32 i32)
    (i32.const 1024)

    (if (param i32) (result i32)
      (i32.eqz (local.get $inputLength))
      (then
        (i32.const 0)
        (return)))

    (call $tailrecEncode
      (i32.const 1024)
      (i32.const 1)
      (i32.load8_s (local.get $inputOffset))
      (i32.add (local.get $inputOffset) (i32.const 1))
      (i32.sub (local.get $inputLength) (i32.const 1)))

    (i32.const 1024)
    (i32.sub))

  ;;
  ;; Checks if the given character is a non-digit character
  ;;
  ;; @param {i32} char - The character to check
  ;;
  ;; @returns {i32} - Zero if the character is a digit, otherwise non-zero
  ;;
  (func $isNonDigit (param $char i32) (result i32)
    (i32.or
      (i32.lt_u (local.get $char) (i32.load8_u (i32.const 0)))
      (i32.gt_u (local.get $char) (i32.load8_u (i32.const 9)))))

  ;;
  ;; Gets the numeric value of a digit character
  ;;
  ;; @param {i32} char - The digit character to convert
  ;;
  ;; @returns {i32} - Numeric value of digit character
  ;;
  (func $digitValue (param $char i32) (result i32)
    (i32.sub (local.get $char) (i32.load8_u (i32.const 0))))

  ;;
  ;; Decodes a character (possibly repeated multiple times) encoded using run-length encoding
  ;;
  ;; @param {i32} offset - The offset where to write the output in linear memory
  ;; @param {i32} count - The number of times the character appeared in the original data
  ;; @param {i32} char - The character to decode
  ;;
  ;; @returns {i32} - The offset past the point where data has been decoded in linear memory
  ;;
  ;; @note A value of 0 for count is treated as 1
  ;;
  (func $decodeChar (param $offset i32) (param $count i32) (param $char i32) (result i32)
    (if (i32.eqz (local.get $count))
      (then
        (local.set $count (i32.const 1))))
    (memory.fill (local.get $offset) (local.get $char) (local.get $count))
    (i32.add (local.get $offset) (local.get $count)))

  ;;
  ;; Recursively decodes a string encoded using run-length encoding
  ;;
  ;; @param {i32} outputOffset - The offset where to write the decoded data in linear memory
  ;; @param {i32} count - The current count of characters being decoded
  ;; @param {i32} inputOffset - The offset of encoded data in linear memory
  ;; @param {i32} inputLength - The length of the encoded data
  ;;
  ;; @param {i32} - The offset past the point where data has been decoded in linear memory
  ;;
  (func $tailrecDecode
    (param $outputOffset i32) (param $count i32) (param $inputOffset i32) (param $inputLength i32)
    (result i32)

    (local $curChar i32)
    
    (local.get $outputOffset)
    (if (param i32) (result i32)
      (i32.eqz (local.get $inputLength))
      (then (return)))

    (local.set $curChar (i32.load8_u (local.get $inputOffset)))
    (local.get $count)
    (if (param i32) (param i32) (result i32 i32)
      (call $isNonDigit (local.get $curChar))
      (then
        (local.get $curChar)
        (call $decodeChar)
        (i32.const 0))
      (else
        (i32.const 10)
        (i32.mul)
        (call $digitValue (local.get $curChar))
        (i32.add)))

    (i32.add (local.get $inputOffset) (i32.const 1))
    (i32.sub (local.get $inputLength) (i32.const 1))
    (call $tailrecDecode)) ;; Could be `return_call` in the future

  ;;
  ;; Decode a string using run-length encoding
  ;;
  ;; @param {i32} inputOffset - The offset of the string in linear memory
  ;; @param {i32} inputLength - The length of the string in linear memory
  ;;
  ;; returns {(i32,i32)} - The offset and length of the decoded string in linear memory
  ;;
  (func (export "decode") (param $inputOffset i32) (param $inputLength i32) (result i32 i32)
    (i32.const 1024)

    (call $tailrecDecode (i32.const 1024) (i32.const 0) (local.get $inputOffset) (local.get $inputLength))

    (i32.const 1024)
    (i32.sub))
)
