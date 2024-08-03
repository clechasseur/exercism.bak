pub fn append(first first: List(a), second second: List(a)) -> List(a) {
  case first {
    [] -> second
    [h, ..t] -> [h, ..append(t, second)]
  }
}

fn tailrec_concat(lists: List(List(a)), acc: List(a)) -> List(a) {
  case lists {
    [] -> acc
    [hl, ..tl] -> append(hl, tailrec_concat(tl, acc))
  }
}

pub fn concat(lists: List(List(a))) -> List(a) {
  tailrec_concat(lists, [])
}

pub fn filter(list: List(a), function: fn(a) -> Bool) -> List(a) {
  case list {
    [] -> []
    [h, ..t] -> case function(h) {
      True -> [h, ..filter(t, function)]
      False -> filter(t, function)
    }
  }
}

pub fn length(list: List(a)) -> Int {
  case list {
    [] -> 0
    [_, ..t] -> 1 + length(t)
  }
}

pub fn map(list: List(a), function: fn(a) -> b) -> List(b) {
  case list {
    [] -> []
    [h, ..t] -> [function(h), ..map(t, function)]
  }
}

pub fn foldl(
  over list: List(a),
  from initial: b,
  with function: fn(b, a) -> b,
) -> b {
  case list {
    [] -> initial
    [h, ..t] -> foldl(t, function(initial, h), function)
  }
}

pub fn foldr(
  over list: List(a),
  from initial: b,
  with function: fn(b, a) -> b,
) -> b {
  case list {
    [] -> initial
    [h, ..t] -> function(foldr(t, initial, function), h)
  }
}

fn tailrec_reverse(list: List(a), acc: List(a)) -> List(a) {
  case list {
    [] -> acc
    [h, ..t] -> tailrec_reverse(t, [h, ..acc])
  }
}

pub fn reverse(list: List(a)) -> List(a) {
  tailrec_reverse(list, [])
}
