(module
  (memory (export "mem") 1)

  ;; Memory block containing the whole colors string
  (data (i32.const 200) "black,brown,red,orange,yellow,green,blue,violet,grey,white")

  ;; Memory blocks containing each color as an individual string, in order of value
  (data (i32.const 300) "black")
  (data (i32.const 310) "brown")
  (data (i32.const 320) "red")
  (data (i32.const 330) "orange")
  (data (i32.const 340) "yellow")
  (data (i32.const 350) "green")
  (data (i32.const 360) "blue")
  (data (i32.const 370) "violet")
  (data (i32.const 380) "grey")
  (data (i32.const 390) "white")

  ;;
  ;; Compares a string stored in memory with a target string
  ;;
  ;; @param {i32} offset - offset of string to compare
  ;; @param {i32} len - length of string to compare
  ;; @param {i32} target - target string to compare it to
  ;;
  ;; @returns {i32} 0 if strings are equal, otherwise non-zero
  ;;
  (func $strcmp (param $offset i32) (param $len i32) (param $target i32) (result i32)
    (local $cmp i32)

    (block $break
      (loop $continue
        (br_if $break (i32.eqz (local.get $len)))

        (local.set $cmp
          (i32.sub (i32.load8_u (local.get $offset)) (i32.load8_u (local.get $target))))
        (local.set $offset
          (i32.add (local.get $offset) (i32.const 1)))
        (local.set $target
          (i32.add (local.get $target) (i32.const 1)))
        (local.set $len
          (i32.sub (local.get $len) (i32.const 1)))

        (br_if $continue (i32.eqz (local.get $cmp)))))

    (local.get $cmp))

  ;;
  ;; Return buffer of comma separated colors
  ;; "black,brown,red,orange,yellow,green,blue,violet,grey,white"
  ;;
  ;; @returns {(i32, i32)} - The offset and length of the buffer of comma separated colors
  ;;
  (func (export "colors") (result i32 i32)
    (i32.const 200) (i32.const 58))

  ;;
  ;; Given a valid resistor color, returns the associated value
  ;;
  ;; @param {i32} offset - offset into the color buffer
  ;; @param {i32} len - length of the color string
  ;;
  ;; @returns {i32} - the associated value
  ;;
  (func (export "colorCode") (param $offset i32) (param $len i32) (result i32)
    (local $target i32)
    (local $cmp i32)

    ;; Color names cannot be longer than 10 bytes currently
    (if (i32.gt_s (local.get $len) (i32.const 10))
      (then
        (unreachable)))

    (local.set $target (i32.const 300))

    (block $break
      (loop $continue
        (br_if $break (i32.gt_s (local.get $target) (i32.const 390)))

        (local.set $cmp
          (call $strcmp (local.get $offset) (local.get $len) (local.get $target)))
        (br_if $break (i32.eqz (local.get $cmp)))

        (local.set $target
          (i32.add (local.get $target) (i32.const 10)))
        (br $continue)))

    (if (result i32)
      (i32.gt_s (local.get $target) (i32.const 390))
      (then
        (i32.const -1))
      (else
        (i32.div_s
          (i32.sub (local.get $target) (i32.const 300))
          (i32.const 10)))))
)
