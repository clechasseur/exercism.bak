(module
  ;;
  ;; count the number of 1 bits in the binary representation of a number
  ;;
  ;; @param {i32} number - the number to count the bits of
  ;;
  ;; @returns {i32} the number of 1 bits in the binary representation of the number
  ;;
  (func (export "eggCount") (param $number i32) (result i32)
    (i32.const 0)
    (loop $loop (param i32) (result i32)
      (i32.add (i32.and (local.get $number) (i32.const 0x1)))
      (br_if $loop
        (local.tee $number (i32.shr_u (local.get $number) (i32.const 1))))))
)
