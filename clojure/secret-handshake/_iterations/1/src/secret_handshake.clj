(ns secret-handshake)

(defn commands [handshake]
  (cond-> []
          (pos? (bit-and handshake 2r1))     (conj "wink")
          (pos? (bit-and handshake 2r10))    (conj "double blink")
          (pos? (bit-and handshake 2r100))   (conj "close your eyes")
          (pos? (bit-and handshake 2r1000))  (conj "jump")
          (pos? (bit-and handshake 2r10000)) reverse))
