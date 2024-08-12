import java.util.*;
import java.util.stream.Collector;

class HandshakeCalculator {
    private static final int REVERSE = 16;

    List<Signal> calculateHandshake(int number) {
        return Arrays.stream(Signal.values())
                     .filter(s -> (number & s.getValue()) != 0)
                     .collect(handshakeCollector((number & REVERSE) != 0));
    }

    private Collector<Signal, ?, List<Signal>> handshakeCollector(boolean reverse) {
        return Collector.of(
                ArrayDeque<Signal>::new,
                (ad, sig) -> { if (reverse) { ad.addFirst(sig); } else { ad.addLast(sig); } },
                (first, second) -> { first.addAll(second); return first; },
                ArrayList::new
        );
    }
}
