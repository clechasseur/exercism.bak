import unicode

proc reverse*(s: string): string =
  var reversedRunes: seq[Rune] = @[]
  for r in s.runes:
    reversedRunes.insert(r, 0)
  $reversedRunes
  # or, more simply:
  # s.reversed  # Uses unicode/reversed

# To test Unicode support in the proc above, use the following test:
#
# test "a string with emojis":
#   check reverse("😅⚙️🦀") == "🦀️⚙😅"
