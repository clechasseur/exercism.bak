function sum_of_multiples(limit, factors)
    sum(∪([zero(limit)], (f:f:limit-1 for f in factors if !iszero(f))...))
end
