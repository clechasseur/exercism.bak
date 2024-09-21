(module
  (func $square_detail (param $squareNum i32) (result i64)
    (i64.const 1)
    (block $loop_break (param i64) (result i64)
      (loop $loop (param i64) (result i64)
        (br_if $loop_break (i32.eq (local.get $squareNum) (i32.const 1)))
        (i64.mul (i64.const 2))
        (local.set $squareNum
          (i32.sub (local.get $squareNum) (i32.const 1)))
        (br $loop))))

  ;;
  ;; Calculate the number of grains of wheat on the nth square of the chessboard
  ;;
  ;; @param {i32} squareNum - The square of the chessboard to calculate the number of grains for
  ;;
  ;; @returns {i64} - The number of grains of wheat on the nth square of the 
  ;;                  chessboard or 0 if the squareNum is invalid. The result
  ;;                  is unsigned.
  ;;
  (func $square (export "square") (param $squareNum i32) (result i64)
    (if (result i64)
      (i32.or
        (i32.lt_s (local.get $squareNum) (i32.const 1))
        (i32.gt_s (local.get $squareNum) (i32.const 64)))
      (then
        (i64.const 0))
      (else
        (call $square_detail (local.get $squareNum)))))

  ;;
  ;; Calculate the sum of grains of wheat across all squares of the chessboard
  ;;
  ;; @returns {i64} - The number of grains of wheat on the entire chessboard.
  ;;                  The result is unsigned.
  ;;
  (func (export "total") (result i64)
    (i64.sub
      (call $square_detail (i32.const 65))
      (i64.const 1)))
)
