function combinations_in_cage(total, numdigits, existing=[])
    perms = unique(permutations(setdiff(1:9, existing)) .|> A -> sort(A[1:numdigits]))
    [perm for perm in perms if sum(perm) == total]
end

# Found this function here:
# https://stackoverflow.com/questions/65051953/julia-generate-all-non-repeating-permutations-in-set-with-duplicates
function permutations(x::T, prefix=T()) where T
    if length(x) == 1
        return [[prefix; x]]
    else
        t = T[]
        for i in eachindex(x)
            if i > firstindex(x) && x[i] == x[i-1]
                continue
            end
            append!(t, permutations([x[begin:i-1];x[i+1:end]], [prefix; x[i]]))
        end
        return t
    end
end
