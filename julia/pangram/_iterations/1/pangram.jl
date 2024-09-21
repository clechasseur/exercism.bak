"""
    ispangram(input)

Return `true` if `input` contains every alphabetic character (case insensitive).

"""
function ispangram(input)
    alphabet = 'a':'z'
    length(intersect(lowercase(input), alphabet)) == length(alphabet)
end
