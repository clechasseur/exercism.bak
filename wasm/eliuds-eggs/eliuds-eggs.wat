(module
  ;;
  ;; count the number of 1 bits in the binary representation of a number
  ;;
  ;; @param {i32} number - the number to count the bits of
  ;;
  ;; @returns {i32} the number of 1 bits in the binary representation of the number
  ;;
  (func (export "eggCount") (param $number i32) (result i32)
    ;; There was a clever implementation here. It's gone now.
    (i32.popcnt (local.get $number)))
)
