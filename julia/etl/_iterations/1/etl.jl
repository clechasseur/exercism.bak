function transform(input::AbstractDict)
    map(collect(input)) do (score, letters)
        map(letter -> lowercase(letter) => score, letters)
    end |> Iterators.flatten |> Dict
end
