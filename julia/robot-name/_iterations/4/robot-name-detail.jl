module _RobotDetail
    export randid!, idtoname

    using Random: shuffle!

    const namespacelen = 26^2 * 10^3
    const availableids = shuffle!(Int32[0:namespacelen-1;])

    randid!() = !isempty(availableids) ? pop!(availableids) : error("Out of robot names")

    function idtoname(id::Int32)
        id, c1 = divrem(id, 26)
        id, c2 = divrem(id, 26)
        id, d1 = divrem(id, 10)
        d3, d2 = divrem(id, 10)
        string('A' + c1, 'A' + c2, d1, d2, d3)
    end
end

