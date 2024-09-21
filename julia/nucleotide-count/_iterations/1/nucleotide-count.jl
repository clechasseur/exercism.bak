"""
    count_nucleotides(strand)

The count of each nucleotide within `strand` as a dictionary.

Invalid strands raise a `DomainError`.

"""
function count_nucleotides(strand)
    if occursin(r"[^ACGT]", strand)
        throw(DomainError(strand, "Invalid DNA strand: $strand"))
    end
    
    counts = Dict(n => 0 for n in "ACGT")
    for n in strand
        counts[n] += 1
    end
    counts
end
