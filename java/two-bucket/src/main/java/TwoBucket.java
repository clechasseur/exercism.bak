import java.util.*;
import java.util.stream.Collectors;

class TwoBucket {
    private int totalMoves;
    private String finalBucket;
    private int otherBucket;

    TwoBucket(int bucketOneCap, int bucketTwoCap, int desired, String startingBucket) {
        List<Set<State>> history = new ArrayList<>();
        history.add(new HashSet<>(Collections.singletonList(getStartingState(bucketOneCap, bucketTwoCap, startingBucket))));
        State illegalState = getIllegalState(bucketOneCap, bucketTwoCap, startingBucket);
        while (!finished(history.get(history.size() - 1), desired)) {
            Set<State> newLayer = new HashSet<>();
            history.get(history.size() - 1).forEach(state -> {
                newLayer.addAll(state.getAllPossibleMoves().stream()
                                                           .filter(subState -> !subState.equals(illegalState))
                                                           .collect(Collectors.toList()));
            });
            history.add(newLayer);
        }
        this.totalMoves = history.size();
    }

    int getTotalMoves() {
        return totalMoves;
    }

    String getFinalBucket() {
        return finalBucket;
    }

    int getOtherBucket() {
        return otherBucket;
    }

    private State getStartingState(int bucketOneCap, int bucketTwoCap, String startingBucket) {
        if (startingBucket.equals("one")) {
            return new State(bucketOneCap, 0, bucketOneCap, bucketTwoCap);
        }
        return new State(0, bucketTwoCap, bucketOneCap, bucketTwoCap);
    }

    private State getIllegalState(int bucketOneCap, int bucketTwoCap, String startingBucket) {
        if (startingBucket.equals("one")) {
            return new State(0, bucketTwoCap, bucketOneCap, bucketTwoCap);
        }
        return new State(bucketOneCap, 0, bucketOneCap, bucketTwoCap);
    }

    private boolean finished(Set<State> states, int desired) {
        Optional<State> doneState = states.stream()
                                          .filter(state -> state.getBucketOne() == desired || state.getBucketTwo() == desired)
                                          .findAny();
        if (doneState.isPresent()) {
            State state = doneState.get();
            if (state.getBucketOne() == desired) {
                this.finalBucket = "one";
                this.otherBucket = state.getBucketTwo();
            } else {
                this.finalBucket = "two";
                this.otherBucket = state.getBucketOne();
            }
            return true;
        }
        return false;
    }

    private static final class State {
        private final int bucketOne;
        private final int bucketTwo;
        private final int bucketOneCap;
        private final int bucketTwoCap;

        State(int bucketOne, int bucketTwo, int bucketOneCap, int bucketTwoCap) {
            this.bucketOne = bucketOne;
            this.bucketTwo = bucketTwo;
            this.bucketOneCap = bucketOneCap;
            this.bucketTwoCap = bucketTwoCap;
        }

        int getBucketOne() {
            return bucketOne;
        }

        int getBucketTwo() {
            return bucketTwo;
        }

        List<State> getAllPossibleMoves() {
            List<State> moves = new ArrayList<>();

            // 1 -> void
            moves.add(new State(0, bucketTwo, bucketOneCap, bucketTwoCap));

            // 2 -> void
            moves.add(new State(bucketOne, 0, bucketOneCap, bucketTwoCap));

            // water -> 1
            moves.add(new State(bucketOneCap, bucketTwo, bucketOneCap, bucketTwoCap));

            // water -> 2
            moves.add(new State(bucketOne, bucketTwoCap, bucketOneCap, bucketTwoCap));

            // 1 -> 2
            int toPour = Math.min(bucketOne, bucketTwoCap - bucketTwo);
            moves.add(new State(bucketOne - toPour, bucketTwo + toPour, bucketOneCap, bucketTwoCap));

            // 2 -> 1
            toPour = Math.min(bucketTwo, bucketOneCap - bucketOne);
            moves.add(new State(bucketOne + toPour, bucketTwo - toPour, bucketOneCap, bucketTwoCap));

            return moves;
        }

        @Override
        public boolean equals(Object obj) {
            return obj instanceof State &&
                    ((State) obj).getBucketOne() == getBucketOne() &&
                    ((State) obj).getBucketTwo() == getBucketTwo();
        }

        @Override
        public int hashCode() {
            return Objects.hash(bucketOne, bucketTwo);
        }
    }
}
