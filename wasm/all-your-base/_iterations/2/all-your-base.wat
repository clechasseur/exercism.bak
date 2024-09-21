(module
  (memory (export "mem") 1)
  
  ;; Status codes returned as res[2]
  (global $ok i32 (i32.const 0))
  (global $inputHasWrongFormat i32 (i32.const -1))
  (global $wrongInputBase i32 (i32.const -2))
  (global $wrongOutputBase i32 (i32.const -3))

  ;;
  ;; Convert an array of digits in inputBase to an array of digits in outputBase
  ;;
  ;; @param {i32} arrOffset - offset of input i32[] array
  ;; @param {i32} arrLength - length of input i32[] array in elements
  ;; @param {i32} inputBase - base of the input array
  ;; @param {i32} outputBase - base of the output array
  ;;
  ;; @return {i32} - offset of the output i32[] array
  ;; @return {i32} - length of the output i32[] array in elements
  ;; @return {i32} - status code (0, -1, -2, -3)                                
  ;;
  (func (export "convert") (param $arrOffset i32) (param $arrLength i32) (param $inputBase i32) (param $outputBase i32) (result i32 i32 i32)
    (local $n i32)
    (local $arrEndOffset i32)

    ;; Check if input base is valid
    (if (i32.lt_s (local.get $inputBase) (i32.const 2))
      (then
        (return (i32.const 0) (i32.const 0) (global.get $wrongInputBase))))

    ;; Check if output base is valid
    (if (i32.lt_s (local.get $outputBase) (i32.const 2))
      (then
        (return (i32.const 0) (i32.const 0) (global.get $wrongOutputBase))))

    ;; In the WebAssembly track, an input array length less than 1 is considered invalid.
    ;; Not all tracks have this requirement.
    (if (i32.le_s (local.get $arrLength) (i32.const 0))
      (then
        (return (i32.const 0) (i32.const 0) (global.get $inputHasWrongFormat))))

    ;; In the WebAssembly track, having leading zeroes is considered invalid.
    ;; Not all tracks have this requirement.
    (if
      (i32.and
        (i32.gt_s (local.get $arrLength) (i32.const 1))
        (i32.eqz (i32.load (local.get $arrOffset))))
      (then
        (return (i32.const 0) (i32.const 0) (global.get $inputHasWrongFormat))))

    ;; Read digits in base $inputBase and convert to base-10, store in $n
    (local.set $n (i32.const 0))
    (local.set $arrEndOffset
      (i32.add (local.get $arrOffset) (i32.mul (local.get $arrLength) (i32.const 4))))
    (loop $continue
      ;; Check if every digit is 0 <= digit < $outputBase
      (if
        (i32.or
          (i32.lt_s (i32.load (local.get $arrOffset)) (i32.const 0))
          (i32.ge_s (i32.load (local.get $arrOffset)) (local.get $inputBase)))
        (then
          (return (i32.const 0) (i32.const 0) (global.get $inputHasWrongFormat))))
      
      (local.set $n
        (i32.add
          (i32.mul (local.get $n) (local.get $inputBase))
          (i32.load (local.get $arrOffset))))
      (local.set $arrOffset (i32.add (local.get $arrOffset) (i32.const 4)))
      (br_if $continue (i32.lt_s (local.get $arrOffset) (local.get $arrEndOffset))))

    ;; If $n is 0, simply return 0.
    (if (i32.eqz (local.get $n))
      (then
        (i32.store (local.get $arrOffset) (i32.const 0))
        (return (local.get $arrOffset) (i32.const 1) (global.get $ok))))

    ;; Convert base-10 number in $n to the $outputBase and store
    ;; the digits in reverse, starting at offset 256 (64 digits * 4 bytes each).
    ;; (64 digits ought to be enough for everybody)
    (local.set $arrOffset (i32.const 256))
    (local.set $arrLength (i32.const 0))
    (loop $continue
      (i32.store (local.get $arrOffset) (i32.rem_s (local.get $n) (local.get $outputBase)))
      (local.set $n (i32.div_s (local.get $n) (local.get $outputBase)))

      (local.set $arrOffset (i32.sub (local.get $arrOffset) (i32.const 4)))
      (local.set $arrLength (i32.add (local.get $arrLength) (i32.const 1)))
      (br_if $continue (i32.gt_s (local.get $n) (i32.const 0))))

    ;; Output digits are stored at address ($arrOffset + 4) and there are $arrLength of them.
    (i32.add (local.get $arrOffset) (i32.const 4))
    (local.get $arrLength)
    (global.get $ok))
)
