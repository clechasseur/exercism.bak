"Square the sum of the first `n` positive integers"
function square_of_sum(n::Integer)::Integer
    # (n * (n + 1) ÷ 2) ^ 2
    # Although the above looks smart, Julia is smart too, and
    # overloads sum for ranges, so this is just as efficient:
    sum(1:n) ^ 2
end

"Sum the squares of the first `n` positive integers"
function sum_of_squares(n::Integer)::Integer
    n * (n + 1) * (2n + 1) ÷ 6
end

"Subtract the sum of squares from square of the sum of the first `n` positive ints"
function difference(n::Integer)::Integer
    square_of_sum(n) - sum_of_squares(n)
end
