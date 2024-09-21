const scores = [(1.0, 10), (5.0, 5), (10.0, 1), (Inf, 0)]

function score(x::Real, y::Real)
    dist = √(x^2 + y^2)
    first(s for (r, s) in scores if dist <= r)
end
