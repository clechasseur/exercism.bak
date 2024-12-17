def maxvalue($capacity; $items):
  $items
  | keys
  | map(. as $i | [$i, $items[$i]])
  | map(
      .[1] as $item
      | select($item.weight <= $capacity)
    )
  | map(
      .[0] as $i
      | .[1] as $item
      | $item.value + maxvalue($capacity - $item.weight; $items | .[($i + 1):])
    )
  | max // 0
;

maxvalue(.maximumWeight; .items)
