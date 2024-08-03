function wordcount(sentence)
    words = eachmatch(r"\d+|[a-z]+('[a-z]+)?", lowercase(sentence))
    if !isempty(words)
        mergewith(+, map(m -> Dict(m.match => 1), words)...)
    else
        Dict()
    end
end

