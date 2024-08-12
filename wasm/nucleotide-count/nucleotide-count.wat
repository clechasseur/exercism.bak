(module
  (memory (export "mem") 1)

  ;; Empty values to use as initial counts
  (data (i32.const 16)
    "\00\00\00\00"
    "\00\00\00\00"
    "\00\00\00\00"
    "\00\00\00\00")

  ;; Values to return when the DNA string is invalid
  (data (i32.const 32)
    "\FF\FF\FF\FF"
    "\FF\FF\FF\FF"
    "\FF\FF\FF\FF"
    "\FF\FF\FF\FF")

  ;; Letters corresponding to each nucleotide type
  (data (i32.const 48) "ACGT")

  ;;
  ;; Given a character for a nucleotide type, attempt to determine
  ;; the kind of nucleotide that it represents.
  ;;
  ;; @param {i32} offset - Offset of character representing nucleotide in memory.
  ;;
  ;; @returns {i32} - 0-based index of nucleotide type, or -1 if invalid nucleotide.
  ;;
  (func $matchNucleotide (param $offset i32) (result i32)
    (local $targetNucleotide i32)
    (local $nucleotideOffset i32)

    (local.set $targetNucleotide (i32.load8_u (local.get $offset)))
    (local.set $nucleotideOffset (i32.const 48))

    (block $break
      (loop $continue
        (br_if $break (i32.eq (local.get $nucleotideOffset) (i32.const 52)))
        (br_if $break (i32.eq (local.get $targetNucleotide) (i32.load8_u (local.get $nucleotideOffset))))

        (local.set $nucleotideOffset (i32.add (local.get $nucleotideOffset) (i32.const 1)))
        (br $continue)))

    (if (result i32)
      (i32.eq (local.get $nucleotideOffset) (i32.const 52))
      (then (i32.const -1))
      (else (i32.sub (local.get $nucleotideOffset) (i32.const 48)))))

  ;;
  ;; Count the number of each nucleotide in a DNA string.
  ;;
  ;; @param {i32} offset - The offset of the DNA string in memory.
  ;; @param {i32} length - The length of the DNA string.
  ;;
  ;; @returns {(i32,i32,i32,i32)} - The number of adenine, cytosine, guanine, 
  ;;                                and thymine nucleotides in the DNA string
  ;;                                or (-1, -1, -1, -1) if the DNA string is
  ;;                                invalid.
  ;;
  (func (export "countNucleotides") (param $offset i32) (param $length i32) (result i32 i32 i32 i32)
    (local $endOffset i32)
    (local $nucleotideOffset i32)
    (local $count i32)

    ;; Offset 0 will contain the counts.
    ;; Initialize the counts to 0 by copying those values from offset 16. 
    (memory.copy (i32.const 0) (i32.const 16) (i32.const 16))

    (local.set $endOffset (i32.add (local.get $offset) (local.get $length)))
    (block $break
      (loop $continue
        (br_if $break (i32.eq (local.get $offset) (local.get $endOffset)))

        ;; Fetch offset of this nucleotide
        (local.set $nucleotideOffset
          (call $matchNucleotide (local.get $offset)))
        (if (i32.ne (local.get $nucleotideOffset) (i32.const -1))
          (then
            ;; Nucleotide found, increment the corresponding count
            (local.set $nucleotideOffset (i32.mul (local.get $nucleotideOffset) (i32.const 4)))
            (local.set $count (i32.load (local.get $nucleotideOffset)))
            (i32.store (local.get $nucleotideOffset) (i32.add (local.get $count) (i32.const 1))))
          (else
            ;; Invalid DNA string - copy invalid values to our counts and bail out
            (memory.copy (i32.const 0) (i32.const 32) (i32.const 16))
            (br $break)))

        (local.set $offset (i32.add (local.get $offset) (i32.const 1)))
        (br $continue)))

    ;; Copy final counts to the return values
    (i32.load (i32.const 0))
    (i32.load (i32.const 4))
    (i32.load (i32.const 8))
    (i32.load (i32.const 12)))
)
