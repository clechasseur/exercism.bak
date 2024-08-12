const actions = ["wink", "double blink", "close your eyes", "jump"]

function isbitset(n, bit)
    n & (1 << bit) != zero(n)
end

function secret_handshake(code)
    handshake = [a for (i, a) in enumerate(actions) if isbitset(code, i - 1)]
    isbitset(code, 4) && reverse!(handshake)
    handshake
end
