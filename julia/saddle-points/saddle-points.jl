function saddlepoints(M)
    if isempty(M)
        return []
    end

    # I must admin I stole the idea from a community solution, but it's pretty sweet. :)
    # Here's how it works, with examples taken from the REPL.
    #
    # Let's start with an input matrix M.
    #
    # julia> M = [3 1 3; 3 2 4]
    # 2×3 Matrix{Int64}:
    #  3  1  3
    #  3  2  4
    #
    # minimum and maximum return matrices of the min or max values for each row/column.
    #
    # julia> col_min = minimum(M, dims=1)
    # 1×3 Matrix{Int64}:
    #  3  1  3
    #
    # julia> row_max = maximum(M, dims=2)
    # 2×1 Matrix{Int64}:
    #  3
    #  4
    #
    # col_min .== row_max vectorizes the == operator over both matrices; because they
    # do not have the same dimensions, they are extended in each dimension by copying
    # the existing rows/columns. This produces a matrix of boolean values, where an
    # element is true only if the value in col_min and row_max are equal - in other
    # words, if it's a saddle point. (This is the genius part)
    #
    # julia> col_min .== row_max
    # 2×3 BitMatrix:
    #  1  0  1
    #  0  0  0
    #
    # findall returns a vector of array indices for elements that are true. The indices
    # are represented as CartesianIndex instances.
    #
    # julia> findall(col_min .== row_max)
    # 2-element Vector{CartesianIndex{2}}:
    #  CartesianIndex(1, 1)
    #  CartesianIndex(1, 3)
    #
    # At this point, we're done - but the tests expect coordinates to be expressed as Tuples.
    # Thus, Tuple. vectorizes the Tuple constructor over the CartesianIndex vector, converting
    # each of them to an equivalent Tuple.
    #
    # julia> Tuple.(findall(col_min .== row_max))
    # 2-element Vector{Tuple{Int64, Int64}}:
    #  (1, 1)
    #  (1, 3)
    #
    col_min = minimum(M, dims=1)
    row_max = maximum(M, dims=2)
    Tuple.(findall(col_min .== row_max))
end
