function rotate(n::Integer, c::AbstractChar)::AbstractChar
    if !isletter(c)
        return c
    end

    root = isuppercase(c) ? 'A' : 'a'
    root + (((c - root) + n) % 26)
end

function rotate(n::Integer, s::AbstractString)::AbstractString
    map(c -> rotate(n, c), s)
end

# Bonus:
for n in 0:26
    name = Symbol("R", n, "_str")
    eval(:(macro $name(s); rotate($n, s); end))
end
