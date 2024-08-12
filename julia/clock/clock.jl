import Base: show, +, -
import Dates: Minute, hour, minute, value
using Printf: @printf

const minutes_in_hour::Int64 = 60
const minutes_in_day::Int64 = minutes_in_hour * 24

struct Clock
    m::Int64
    Clock(m) = new(mod(m, minutes_in_day))
end

Clock(h, m) = Clock(h * minutes_in_hour + m)

hour(c::Clock) = c.m ÷ minutes_in_hour
minute(c::Clock) = c.m % minutes_in_hour

+(c::Clock, m::Minute) = Clock(c.m + value(m))
-(c::Clock, m::Minute) = c + -m

show(io::IO, c::Clock) = @printf io "\"%02i:%02i\"" hour(c) minute(c)
