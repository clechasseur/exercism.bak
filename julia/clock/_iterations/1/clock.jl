using Dates, Printf

const minutes_in_hour::Int64 = 60
const minutes_in_day::Int64 = minutes_in_hour * 24

struct Clock
    m::Int64
    Clock(m::Integer) = new(mod(m, minutes_in_day))
end

Clock(h::Integer, m::Integer) = Clock(h * minutes_in_hour + m)

h(c::Clock) = c.m ÷ minutes_in_hour
m(c::Clock) = c.m % minutes_in_hour

Base.:(==)(c1::Clock, c2::Clock) = c1.m == c2.m
Base.:<(c1::Clock, c2::Clock) = c1.m < c2.m

Base.:+(c::Clock, m::Dates.Minute) = Clock(c.m + Dates.value(m))
Base.:-(c::Clock, m::Dates.Minute) = Clock(c.m - Dates.value(m))

Base.show(io::IO, c::Clock) = @printf io "\"%02i:%02i\"" h(c) m(c)
