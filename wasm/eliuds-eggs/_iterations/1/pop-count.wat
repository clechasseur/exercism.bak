(module
  ;;
  ;; count the number of 1 bits in the binary representation of a number
  ;;
  ;; @param {i32} number - the number to count the bits of
  ;;
  ;; @returns {i32} the number of 1 bits in the binary representation of the number
  ;;
  (func (export "eggCount") (param $number i32) (result i32)
    (local $eggs i32)

    (local.set $eggs (i32.const 0))
    (block $loop_break
      (loop $loop
        (br_if $loop_break (i32.eq (local.get $number) (i32.const 0)))
        (if (i32.and (local.get $number) (i32.const 0x1))
          (then
            (local.set $eggs
              (i32.add (local.get $eggs) (i32.const 1)))))
        (local.set $number
          (i32.shr_u (local.get $number) (i32.const 1)))
        (br $loop)))
    (local.get $eggs))
)
