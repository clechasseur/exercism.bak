.series as $digits
| ($digits | length) as $len
| .sliceLength as $slen
| if $len == 0 then ("series cannot be empty" | halt_error)
  elif $slen > $len then ("slice length cannot be greater than series length" | halt_error)
  elif $slen == 0 then ("slice length cannot be zero" | halt_error)
  elif $slen < 0 then ("slice length cannot be negative" | halt_error)
  else [range(0; $len - $slen + 1)] | map($digits[. : . + $slen])
  end
