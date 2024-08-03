function to_roman(number)
    (number > 0 && number < 4000) || error("$number cannot be converted to roman")

    parts = (roman_digit(d, roman_infos[i]) for (i, d) in enumerate(digits(number)))
    parts |> Iterators.reverse |> join
end

struct RomanInfo
    one::Char
    five::Char
    ten::Char
end

const roman_infos = [
    RomanInfo('I', 'V', 'X'),
    RomanInfo('X', 'L', 'C'),
    RomanInfo('C', 'D', 'M'),
    RomanInfo('M', '!', '!')
]

function roman_digit(digit, infos::RomanInfo)
    if digit < 4
        infos.one ^ digit
    elseif digit == 4
        string(infos.one, infos.five)
    elseif digit < 9
        string(infos.five, infos.one ^ (digit - 5))
    else
        string(infos.one, infos.ten)
    end
end
