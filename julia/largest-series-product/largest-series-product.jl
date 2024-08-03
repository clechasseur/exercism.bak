function largest_product(str, span)
    if span < 0
        throw(ArgumentError("Span must be greater than or equal to 0, was $span"))
    elseif length(str) < span
        throw(ArgumentError("Length of \"$str\" must be greater than or equal to span $span"))
    end

    digits = [parse(Int, c) for c in str]
    ((@view digits[i:(i + span - 1)]) for i in 1:(length(str) - span + 1)) |> w -> prod.(w) |> maximum
end
