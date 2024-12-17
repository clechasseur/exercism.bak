function triangle(n)
    if n < 0
        throw(DomainError("Only non-negative numbers allowed"))
    elseif n == 0
        return []
    elseif n == 1
        return [[1]]
    end

    a = triangle(n - 1)
    lastrow = last(a)
    nextrow = [1; [a + b for (a, b) in Iterators.zip(lastrow, Iterators.drop(lastrow, 1))]; 1]
    [a; [nextrow]]
end
