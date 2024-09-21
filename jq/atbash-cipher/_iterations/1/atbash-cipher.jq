def reverse_char:
  ["a", ., "z"] | map(explode) | flatten | [.[2] - (.[1] - .[0])] | implode
;

def reverse:
  gsub("[^[:alnum:]]+"; "")
  | gsub("(?<c>[[:alpha:]])"; "\(.c | ascii_downcase | reverse_char)")
;

def encode:
  reverse
  | [scan(".{1,5}")]
  | join(" ")
;

def decode:
  reverse
;

if .property == "encode" then
  .input.phrase | encode
else
  .input.phrase | decode
end
