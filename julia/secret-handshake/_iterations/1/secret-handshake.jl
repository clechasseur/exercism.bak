const actions = ["wink", "double blink", "close your eyes", "jump"]

function secret_handshake(code)
    handshake = [actions[i] for i in eachindex(actions) if code & (0b1 << (i - 1)) != 0]
    if code & 0b10000 != 0
        reverse!(handshake)
    end
    handshake
end
