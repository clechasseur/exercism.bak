import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

class BowlingGame {
    private List<Frame> frames = new ArrayList<>(Collections.singletonList(new RegularFrame()));
    private boolean gameComplete;
    private RuntimeException error;

    void roll(int pins) {
        try {
            if (gameComplete) {
                throw new IllegalStateException("Cannot roll after game is over");
            }

            boolean done = frames.get(frames.size() - 1).roll(pins);
            if (done) {
                if (frames.size() == 10) {
                    gameComplete = true;
                } else if (frames.size() == 9) {
                    frames.add(new LastFrame());
                } else {
                    frames.add(new RegularFrame());
                }
            }
        } catch (RuntimeException e) {
            if (error == null) {
                error = e;
            }
        }
    }

    int score() {
        if (error != null) {
            throw error;
        } else if (!gameComplete) {
            throw new IllegalStateException("Score cannot be taken until the end of the game");
        }

        return frames.stream()
                     .mapToInt(frame -> frame.score(frames.stream()
                                                          .skip(frames.indexOf(frame) + 1)
                                                          .collect(Collectors.toList())))
                     .sum();
    }

    private interface Frame {
        boolean roll(int pins);
        int score(List<Frame> nextFrames);
        List<Integer> rolls();
    }

    private static class RegularFrame implements Frame {
        private List<Integer> rolls = new ArrayList<>();
        private int maxRolls;

        RegularFrame() {
            this.maxRolls = 2;
        }

        RegularFrame(int maxRolls) {
            this.maxRolls = maxRolls;
        }

        public boolean roll(int pins) {
            if (pins < 0) {
                throw new IllegalStateException("Negative roll is invalid");
            } else if (this.rolls.stream().mapToInt(i -> i).sum() + pins > 10) {
                throw new IllegalStateException("Pin count exceeds pins on the lane");
            }

            this.rolls.add(pins);
            return this.rolls.size() == maxRolls || this.rolls.get(0) == 10;
        }

        public int score(List<Frame> nextFrames) {
            if (this.rolls.size() == 1 && this.rolls.get(0) == 10) {
                int nextRoll1 = 0;
                int nextRoll2 = 0;
                if (nextFrames.size() >= 1) {
                    nextRoll1 = nextFrames.get(0).rolls().get(0);
                    if (nextFrames.get(0).rolls().size() >= 2) {
                        nextRoll2 = nextFrames.get(0).rolls().get(1);
                    } else if (nextFrames.size() >= 2) {
                        nextRoll2 = nextFrames.get(1).rolls().get(0);
                    }
                }
                return 10 + nextRoll1 + nextRoll2;
            } else if (this.rolls.stream().mapToInt(i -> i).sum() == 10) {
                return 10 + (nextFrames.size() >= 1 ? nextFrames.get(0).rolls().get(0) : 0);
            } else {
                return this.rolls.stream().mapToInt(i -> i).sum();
            }
        }

        public List<Integer> rolls() {
            return Collections.unmodifiableList(this.rolls);
        }
    }

    private static class LastFrame implements Frame {
        private List<Frame> subFrames = new ArrayList<>(Collections.singletonList(new RegularFrame()));

        public boolean roll(int pins) {
            boolean done = subFrames.get(subFrames.size() - 1).roll(pins);
            if (done && canAddExtraFrame()) {
                subFrames.add(new RegularFrame(3 - subFrames.stream().mapToInt(frame -> frame.rolls().size()).sum()));
                done = false;
            }
            return done;
        }

        public int score(List<Frame> nextFrames) {
            return subFrames.stream().mapToInt(frame -> frame.score(Collections.emptyList())).sum();
        }

        public List<Integer> rolls() {
            return subFrames.stream().flatMap(frame -> frame.rolls().stream()).collect(Collectors.toList());
        }

        private boolean canAddExtraFrame() {
            return subFrames.size() < 3 &&
                    subFrames.stream().mapToInt(frame -> frame.score(Collections.emptyList())).sum() >= 10 &&
                    subFrames.stream().mapToInt(frame -> frame.rolls().size()).sum() <= 2;
        }
    }
}
