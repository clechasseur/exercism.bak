(ns secret-handshake)

(defn commands [handshake]
  (cond-> []
          (bit-test handshake 0) (conj "wink")
          (bit-test handshake 1) (conj "double blink")
          (bit-test handshake 2) (conj "close your eyes")
          (bit-test handshake 3) (conj "jump")
          (bit-test handshake 4) reverse))
