function transform(input::AbstractDict)
    Dict(
        lowercase(letter) => score
        for (score, letters) in input
        for letter in letters
    )
end
