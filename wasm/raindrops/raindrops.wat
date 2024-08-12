(module
  (memory (export "mem") 1)

  (data (i32.const 300) "Pling")
  (data (i32.const 500) "Plang")
  (data (i32.const 700) "Plong")

  (data (i32.const 1000) "0")

  ;;
  ;; Checks if number n is divisible by factor and if so, copies appropriate sound to dest offset
  ;;
  ;; @param {i32} dst - offset where to copy the sound
  ;; @param {i32} n - number to check
  ;; @param {i32} factor - factor to divide n by
  ;;
  ;; @returns {i32} new destination offset after copy (if any)
  ;;
  (func $checkFactor (param $dst i32) (param $n i32) (param $factor i32) (result i32)
    (if (result i32)
      (i32.eqz (i32.rem_u (local.get $n) (local.get $factor)))
      (then
        (memory.copy (local.get $dst) (i32.mul (local.get $factor) (i32.const 100)) (i32.const 5))
        (i32.add (local.get $dst) (i32.const 5)))
      (else
        (local.get $dst))))

  ;;
  ;; Converts a number to a string (in base 10).
  ;;
  ;; Note: because it doesn't happen in this exercise, this function purposefully
  ;; does not attempt to correctly handle 0 or negative numbers.
  ;;
  ;; @param {i32} n - number to convert
  ;; @param {i32} dst - offset where to output the string version of n
  ;;
  ;; @returns {i32} new destination offset after conversion
  ;;
  (func $itoa (param $n i32) (param $dst i32) (result i32)
    (local.get $n)
    (loop $continue
      (local.set $dst (i32.add (local.get $dst) (i32.const 1)))
      (br_if $continue
        (i32.ne (local.tee $n (i32.div_s (local.get $n) (i32.const 10))) (i32.const 0))))

    (local.set $n)
    (local.get $dst)
    (loop $continue
      (i32.store8
        (local.tee $dst (i32.sub (local.get $dst) (i32.const 1)))
        (i32.add (i32.load8_u (i32.const 1000)) (i32.rem_s (local.get $n) (i32.const 10))))
      (br_if $continue
        (i32.ne (local.tee $n (i32.div_s (local.get $n) (i32.const 10))) (i32.const 0)))))

  ;;
  ;; Convert a number into a string of raindrop sounds
  ;;
  ;; @param {i32} input - The number to convert
  ;;
  ;; @returns {(i32,i32)} - Offset and length of raindrop sounds string 
  ;;                        in linear memory
  ;;
  (func (export "convert") (param $input i32) (result i32 i32)
    (local $len i32)

    (i32.const 0)
    (call $checkFactor (local.get $input) (i32.const 3))
    (call $checkFactor (local.get $input) (i32.const 5))
    (call $checkFactor (local.get $input) (i32.const 7))

    (local.set $len)
    (i32.const 0)
    (if (result i32)
      (i32.eqz (local.get $len))
      (then
        (call $itoa (local.get $input) (i32.const 0)))
      (else
        (local.get $len))))
)
