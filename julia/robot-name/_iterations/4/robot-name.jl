include("robot-name-detail.jl")

using ._RobotDetail

mutable struct Robot
    id::Int32
    Robot() = new(randid!())
end

function reset!(robot::Robot)
    robot.id = randid!()
    robot
end

name(robot::Robot) = idtoname(robot.id)
