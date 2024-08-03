def reverse_char:
  ["a", ., "z"] | map(explode) | flatten | [.[2] - (.[1] - .[0])] | implode
;

def reverse:
  gsub("[^[:alnum:]]+"; "")
  | gsub("(?<c>[[:alpha:]])"; "\(.c | reverse_char)")
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
  .input.phrase | ascii_downcase | encode
else
  .input.phrase | ascii_downcase | decode
end
