function combinations_in_cage(total, numdigits, existing=[])
    ps = powerset(setdiff(1:9, existing))
    [s for s in ps if length(s) == numdigits && sum(s) == total] |> sort
end

function powerset(s)
    result = [[]]
    for e in s, i in eachindex(result)
        push!(result, [result[i]; e])
    end
    result
end
