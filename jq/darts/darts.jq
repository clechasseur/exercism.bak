def score:
  if . <= 1.0 then 10
  elif . <= 5.0 then 5
  elif . <= 10.0 then 1
  else 0
  end
;

hypot(.x; .y)
| score
