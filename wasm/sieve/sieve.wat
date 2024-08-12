(module
  (memory (export "mem") 1)

  ;;
  ;; Initializes the region used to store marked non-prime numbers.
  ;;
  (func $initBits
    (memory.fill (i32.const 0) (i32.const 0) (i32.const 1024)))

  ;;
  ;; Checks if the given number has been marked as being non-prime
  ;;
  ;; @param {i32} n - number to check
  ;;
  ;; @return {i32} - whether n is marked as non-prime
  ;;
  (func $checkMarked (param $n i32) (result i32)
    (local $bits i32)

    ;; First bit represents number 2
    (local.set $n (i32.sub (local.get $n) (i32.const 2)))
    (local.set $bits (i32.div_u (local.get $n) (i32.const 8)))
    (local.set $n (i32.rem_u (local.get $n) (i32.const 8)))

    (i32.and
      (i32.load8_u (local.get $bits))
      (i32.shl (i32.const 1) (local.get $n))))

  ;;
  ;; Marks the given as non-prime
  ;;
  ;; @param {i32} n - number to mark as non-prime
  ;;
  (func $mark (param $n i32)
    (local $bits i32)

    ;; First bit represents number 2
    (local.set $n (i32.sub (local.get $n) (i32.const 2)))

    ;; Sanity check: since we reserved only 1024 bytes for our bitfield,
    ;; we can't mark more than 8192 numbers.
    (if (i32.gt_u (local.get $n) (i32.const 8192))
      (then
        (unreachable)))
    
    (local.set $bits (i32.div_u (local.get $n) (i32.const 8)))
    (local.set $n (i32.rem_u (local.get $n) (i32.const 8)))

    (i32.store8
      (local.get $bits)
      (i32.or
        (i32.load8_u (local.get $bits))
        (i32.shl (i32.const 1) (local.get $n)))))

  ;;
  ;; Determine all the prime numbers below a given limit.
  ;; Return the offset and length of the resulting array of primes.
  ;;
  ;; @param {i32} limit - the upper bound for the prime numbers
  ;;
  ;; @return {i32} - offset off the i32[] array
  ;; @return {i32} - length off the i32[] array in elements
  ;;
  (func (export "primes") (param $limit i32) (result i32 i32)
    (local $primes i32)
    (local $candidate i32)
    (local $marker i32)
    
    (call $initBits)
    (local.set $primes (i32.const 1024))

    (local.set $candidate (i32.const 2))
    (block $primesBreak
      (loop $primesContinue
        (br_if $primesBreak (i32.gt_s (local.get $candidate) (local.get $limit)))

        (if (i32.eqz (call $checkMarked (local.get $candidate)))
          (then
            (i32.store (local.get $primes) (local.get $candidate))
            (local.set $primes (i32.add (local.get $primes) (i32.const 4)))

            (local.set $marker (i32.mul (local.get $candidate) (i32.const 2)))
            (block $markBreak
              (loop $markContinue
                (br_if $markBreak (i32.gt_s (local.get $marker) (local.get $limit)))

                (call $mark (local.get $marker))
                (local.set $marker (i32.add (local.get $marker) (local.get $candidate)))
                (br $markContinue)))))

        (local.set $candidate (i32.add (local.get $candidate) (i32.const 1)))
        (br $primesContinue)))

    (i32.const 1024)
    (i32.div_s (i32.sub (local.get $primes) (i32.const 1024)) (i32.const 4)))
)
