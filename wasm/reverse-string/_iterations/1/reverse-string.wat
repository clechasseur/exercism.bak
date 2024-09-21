(module
  (memory (export "mem") 1)
 
  ;;
  ;; Reverse a string
  ;;
  ;; @param {i32} offset - The offset of the input string in linear memory
  ;; @param {i32} length - The length of the input string in linear memory
  ;;
  ;; @returns {(i32,i32)} - The offset and length of the reversed string in linear memory
  ;;
  (func (export "reverseString") (param $offset i32) (param $length i32) (result i32 i32)
    (local $midOffset i32)
    (local $rOffset i32)

    ;; Fun fact: since return values live on the stack, you can actually push them there
    ;; at the start of the function.
    (local.get $offset) (local.get $length)
 
    (local.set $midOffset
      (i32.add (local.get $offset) (i32.div_u (local.get $length) (i32.const 2))))
    (local.set $rOffset
      (i32.sub (i32.add (local.get $offset) (local.get $length)) (i32.const 1)))

    (block $break
      (loop $continue
        (br_if $break (i32.eq (local.get $offset) (local.get $midOffset)))

        ;; Another fun fact: you don't need a temporary variable to swap two
        ;; bytes in memory if you have a stack.
        (local.get $rOffset)
        (i32.load8_u (local.get $offset))
        (i32.store8 (local.get $offset) (i32.load8_u (local.get $rOffset)))
        (i32.store8)

        (local.set $offset (i32.add (local.get $offset) (i32.const 1)))
        (local.set $rOffset (i32.sub (local.get $rOffset) (i32.const 1)))
        (br $continue))))
)
