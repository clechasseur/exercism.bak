function saddlepoints(M)
    collect(c for c in Tuple.(keys(M)) if issaddlepoint(M, c))
end

function issaddlepoint(M, (y, x))
    val = M[y, x]
    all(y′ -> M[y′, x] >= val, axes(M, 1)) && all(x′ -> M[y, x′] <= val, axes(M, 2))
end
