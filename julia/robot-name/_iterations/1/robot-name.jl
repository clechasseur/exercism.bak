using Random: randstring

mutable struct Robot
    name::String
    Robot() = new(randomname())
end

function reset!(robot::Robot)
    robot.name = randomname()
    nothing
end

name(robot::Robot) = robot.name

const usednames = Set{String}()
const namespacelen = 26 * 26 * 10 * 10 * 10

function randomname()
    # Although unlikely, we might run out of unique names
    if length(usednames) == namespacelen
        error("Out of robot names")
    end

    while true
        name = randstring('A':'Z', 2) * randstring('0':'9', 3)
        if name ∉ usednames
            push!(usednames, name)
            return name
        end
    end
end
