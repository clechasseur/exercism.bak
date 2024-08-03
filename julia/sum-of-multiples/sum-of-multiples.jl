function sum_of_multiples(limit, factors)
    multiples = (f:f:limit-1 for f in factors if !iszero(f))
    multiples |> Iterators.flatten |> unique |> a -> sum(a; init=zero(limit))
end
