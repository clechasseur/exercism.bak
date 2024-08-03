(module
  (memory (export "mem") 1)

  ;; Quick reference to the min/max letter values
  (data (i32.const 0) "AZaz")

  ;;
  ;; If the byte at the given offset is between the min/max values provided, encrypt it
  ;; using the provided shift key. Min/max values need to correspond to A and Z (upper or lowercase).
  ;;
  ;; @param {i32} offset - Offset of byte to encrypt in memory.
  ;; @param {i32} min - Min value (e.g. value of A).
  ;; @param {i32} max - Max value (e.g. value of Z).
  ;; @param {i32} shiftKey - The shift key to use for the rotational cipher.
  ;;
  (func $rotateOneBetween (param $offset i32) (param $min i32) (param $max i32) (param $shiftKey i32)
    (local $byte i32)

    (local.set $byte (i32.load8_u (local.get $offset)))

    (if
      (i32.and
        (i32.ge_s (local.get $byte) (local.get $min))
        (i32.le_s (local.get $byte) (local.get $max)))
      (then
        (local.set $byte (i32.add (local.get $byte) (local.get $shiftKey)))
        (if (i32.gt_s (local.get $byte) (local.get $max))
          (then (local.set $byte (i32.sub (local.get $byte) (i32.const 26)))))
        (i32.store8 (local.get $offset) (local.get $byte)))))

  ;;
  ;; Encrypt the byte at the given offset using the rotational cipher. Only encrypts letters.
  ;;
  ;; @param {i32} offset - Offset of byte to encrypt in memory.
  ;; @param {i32} shiftKey - The shift key to use for the rotational cipher.
  ;;
  (func $rotateOne (param $offset i32) (param $shiftKey i32)
    (call $rotateOneBetween (local.get $offset) (i32.load8_u (i32.const 0)) (i32.load8_u (i32.const 1)) (local.get $shiftKey))
    (call $rotateOneBetween (local.get $offset) (i32.load8_u (i32.const 2)) (i32.load8_u (i32.const 3)) (local.get $shiftKey)))

  ;;
  ;; Encrypt plaintext using the rotational cipher.
  ;;
  ;; @param {i32} textOffset - The offset of the plaintext input in linear memory.
  ;; @param {i32} textLength - The length of the plaintext input in linear memory.
  ;; @param {i32} shiftKey - The shift key to use for the rotational cipher.
  ;;
  ;; @returns {(i32,i32)} - The offset and length of the ciphertext output in linear memory.
  ;;
  (func (export "rotate") (param $textOffset i32) (param $textLength i32) (param $shiftKey i32) (result i32 i32)
    (local $offset i32)
    (local $endOffset i32)

    (local.set $offset (local.get $textOffset))
    (local.set $endOffset (i32.add (local.get $offset) (local.get $textLength)))

    (block $break
      (loop $continue
        (br_if $break (i32.eq (local.get $offset) (local.get $endOffset)))

        (call $rotateOne (local.get $offset) (local.get $shiftKey))
        (local.set $offset (i32.add (local.get $offset) (i32.const 1)))
        (br $continue)))

    (local.get $textOffset) (local.get $textLength))
)
