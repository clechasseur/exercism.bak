function sum_of_multiples(limit, factors)
    if isempty(factors)
        return 0
    end

    sum(n for n in 1:(limit - 1) if any(@. factors > 0 && n % factors == 0))
end
