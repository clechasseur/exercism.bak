const sounds = [(3, "Pling"), (5, "Plang"), (7, "Plong")]

function raindrops(n)
    r = join(s for (f, s) in sounds if n % f == 0)
    isempty(r) ? string(n) : r
end

