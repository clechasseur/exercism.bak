"""
    count_nucleotides(strand)

The count of each nucleotide within `strand` as a dictionary.

Invalid strands raise a `DomainError`.

"""
function count_nucleotides(strand)
    counts = Dict(n => 0 for n in "ACGT")
    for n in strand
        haskey(counts, n) || throw(DomainError(strand, "Invalid DNA strand: $strand"))
        counts[n] += 1
    end
    counts
end
