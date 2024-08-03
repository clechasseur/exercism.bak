fun append ([], l) = l
  | append (x::xs, l) = x :: append (xs, l)

fun concat [] = []
  | concat (l::ls) = append (l, concat ls)

fun reverse l =
  let fun tailrec_reverse ([], acc) = acc
        | tailrec_reverse (x::xs, acc) = tailrec_reverse (xs, x :: acc)
  in tailrec_reverse (l, [])
  end

fun filter (_, []) = []
  | filter (f, x::xs) = if f x then x :: filter (f, xs) else filter (f, xs)

fun map (_, []) = []
  | map (f, x::xs) = f x :: map (f, xs)

fun length [] = 0
  | length (_::xs) = 1 + length xs

fun foldl (_, acc, []) = acc
  | foldl (f, acc, x::xs) = foldl (f, f (acc, x), xs)

fun foldr (_, acc, []) = acc
  | foldr (f, acc, x::xs) = f (x, foldr (f, acc, xs))
