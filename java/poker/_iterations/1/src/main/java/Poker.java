import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class Poker {
    private final List<Hand> hands;

    Poker(final List<String> hands) {
        this.hands = hands.stream()
                .map(Hand::new)
                .sorted()
                .collect(Collectors.toList());
    }

    List<String> getBestHands() {
        if (hands.isEmpty()) {
            return Collections.emptyList();
        }
        Hand bestHand = hands.get(hands.size() - 1);
        return hands.stream()
                .filter(hand -> hand.compareTo(bestHand) == 0)
                .map(Hand::toString)
                .collect(Collectors.toList());
    }

    private enum CardSuit {
        CLUBS('C'),
        DIAMONDS('D'),
        HEARTS('H'),
        SPADES('S');

        private static final CardSuit[] VALUES = values();

        private final char suitChar;

        CardSuit(final char suitChar) {
            this.suitChar = suitChar;
        }

        char getSuitChar() {
            return suitChar;
        }

        static CardSuit forSuitChar(char suitChar) {
            for (CardSuit suit : VALUES) {
                if (suit.getSuitChar() == suitChar) {
                    return suit;
                }
            }
            throw new IllegalArgumentException("Invalid suit char: " + suitChar);
        }
    }

    private enum CardValue {
        TWO("2"),
        THREE("3"),
        FOUR("4"),
        FIVE("5"),
        SIX("6"),
        SEVEN("7"),
        EIGHT("8"),
        NINE("9"),
        TEN("10"),
        JACK("J"),
        QUEEN("Q"),
        KING("K"),
        ACE("A");

        private static final CardValue[] VALUES = values();

        private final String valueString;

        CardValue(final String valueString) {
            this.valueString = valueString;
        }

        String getValueString() {
            return valueString;
        }

        static CardValue forValueString(String valueString) {
            for (CardValue value : VALUES) {
                if (value.getValueString().equals(valueString)) {
                    return value;
                }
            }
            throw new IllegalArgumentException("Invalid card value string: " + valueString);
        }
    }

    private enum HandType {
        HIGH_CARD,
        PAIR,
        TWO_PAIRS,
        THREE_OF_A_KIND,
        STRAIGHT,
        FLUSH,
        FULL_HOUSE,
        FOUR_OF_A_KIND,
        STRAIGHT_FLUSH,
    }

    private static final class Card implements Comparable<Card> {
        private final CardSuit suit;
        private final CardValue value;

        Card(final String cardAsString) {
            suit = CardSuit.forSuitChar(cardAsString.charAt(cardAsString.length() - 1));
            value = CardValue.forValueString(cardAsString.substring(0, cardAsString.length() - 1));
        }

        CardSuit getSuit() {
            return suit;
        }

        CardValue getValue() {
            return value;
        }

        @Override
        public String toString() {
            return value.getValueString() + suit.getSuitChar();
        }

        @Override
        public int compareTo(Card o) {
            return value.ordinal() - o.value.ordinal();
        }
    }

    private static final class Hand implements Comparable<Hand> {
        private List<Card> cardsAsProvided;
        private List<Card> sortedCards;
        private HandType handType = null;
        private List<Card> highCards = null;
        private List<Card> nonContributingCards = Collections.emptyList();

        Hand(final String handAsString) {
            cardsAsProvided = Arrays.stream(handAsString.split(" "))
                    .map(Card::new)
                    .collect(Collectors.toList());
            sortedCards = new ArrayList<>(cardsAsProvided);
            sortedCards.sort(null);

            boolean sequence = IntStream.iterate(0, i -> i < sortedCards.size() - 1, i -> i + 1)
                    .map(i -> sortedCards.get(i).compareTo(sortedCards.get(i + 1)))
                    .allMatch(cmp -> cmp == -1);
            boolean alternateSequence = !sequence && sortedCards.stream().map(Card::getValue)
                    .collect(Collectors.toList()).equals(Arrays.asList(
                            CardValue.TWO, CardValue.THREE, CardValue.FOUR, CardValue.FIVE, CardValue.ACE));
            if (sequence || alternateSequence) {
                if (sequence) {
                    highCards = Collections.singletonList(sortedCards.get(sortedCards.size() - 1));
                } else {
                    highCards = Collections.singletonList(sortedCards.get(sortedCards.size() - 2));
                }
                if (sortedCards.stream().allMatch(c -> c.getSuit() == highCards.get(0).getSuit())) {
                    handType = HandType.STRAIGHT_FLUSH;
                } else {
                    handType = HandType.STRAIGHT;
                }
            } else if (sortedCards.stream().allMatch(c -> c.getSuit() == sortedCards.get(0).getSuit())) {
                handType = HandType.FLUSH;
                highCards = Collections.singletonList(sortedCards.get(sortedCards.size() - 1));
            } else {
                List<Integer> pairs = new ArrayList<>();
                List<Integer> threeOfAKind = new ArrayList<>();
                int i = 0;
                while (i < sortedCards.size()) {
                    int j = i + 1;
                    while (j < sortedCards.size() && sortedCards.get(j).compareTo(sortedCards.get(i)) == 0) {
                        j++;
                    }
                    int distance = j - i;
                    if (distance == 5) {
                        throw new IllegalArgumentException("Invalid poker hand: five of the same cards. Cheater!");
                    } else if (distance == 4) {
                        handType = HandType.FOUR_OF_A_KIND;
                        highCards = Collections.singletonList(sortedCards.get(i));
                        if (i == 0) {
                            nonContributingCards = Collections.singletonList(sortedCards.get(sortedCards.size() - 1));
                        } else {
                            nonContributingCards = Collections.singletonList(sortedCards.get(0));
                        }
                        break;
                    } else if (distance == 3) {
                        threeOfAKind.add(i);
                    } else if (distance == 2) {
                        pairs.add(i);
                    }
                    i += distance;
                }
                if (handType != HandType.FOUR_OF_A_KIND) {
                    if (!threeOfAKind.isEmpty()) {
                        if (!pairs.isEmpty()) {
                            handType = HandType.FULL_HOUSE;
                            highCards = Arrays.asList(sortedCards.get(threeOfAKind.get(0)), sortedCards.get(pairs.get(0)));
                        } else {
                            handType = HandType.THREE_OF_A_KIND;
                            highCards = Collections.singletonList(sortedCards.get(threeOfAKind.get(0)));
                            nonContributingCards = getNonContributingCards(sortedCards, Arrays.asList(
                                    threeOfAKind.get(0), threeOfAKind.get(0) + 1, threeOfAKind.get(0) + 2));
                        }
                    } else if (!pairs.isEmpty()) {
                        if (pairs.size() == 2) {
                            handType = HandType.TWO_PAIRS;
                            if (sortedCards.get(pairs.get(0)).compareTo(sortedCards.get(pairs.get(1))) < 0) {
                                highCards = Arrays.asList(sortedCards.get(pairs.get(1)), sortedCards.get(pairs.get(0)));
                            } else {
                                highCards = Arrays.asList(sortedCards.get(pairs.get(0)), sortedCards.get(pairs.get(1)));
                            }
                            nonContributingCards = getNonContributingCards(sortedCards, Arrays.asList(
                                    pairs.get(0), pairs.get(0) + 1, pairs.get(1), pairs.get(1) + 1));
                        } else {
                            handType = HandType.PAIR;
                            highCards = Collections.singletonList(sortedCards.get(pairs.get(0)));
                            nonContributingCards = getNonContributingCards(sortedCards, Arrays.asList(
                                    pairs.get(0), pairs.get(0) + 1));
                        }
                    }
                }
            }

            if (handType == null) {
                handType = HandType.HIGH_CARD;
                highCards = Collections.singletonList(sortedCards.get(sortedCards.size() - 1));
                nonContributingCards = sortedCards.subList(0, sortedCards.size() - 1);
            }
        }

        @Override
        public String toString() {
            return cardsAsProvided.stream()
                    .map(Card::toString)
                    .collect(Collectors.joining(" "));
        }

        @Override
        public int compareTo(Hand o) {
            int cmp = handType.ordinal() - o.handType.ordinal();
            if (cmp != 0) {
                return cmp;
            }

            for (int i = 0; i < highCards.size(); i++) {
                cmp = highCards.get(i).getValue().ordinal() - o.highCards.get(i).getValue().ordinal();
                if (cmp != 0) {
                    return cmp;
                }
            }

            for (int i = nonContributingCards.size() - 1; i >= 0; i--) {
                cmp = nonContributingCards.get(i).getValue().ordinal() - o.nonContributingCards.get(i).getValue().ordinal();
                if (cmp != 0) {
                    return cmp;
                }
            }

            return 0;
        }

        private static List<Card> getNonContributingCards(List<Card> cards, List<Integer> except) {
            return IntStream.iterate(0, i -> i < cards.size(), i -> i + 1)
                    .filter(i -> !except.contains(i))
                    .mapToObj(cards::get)
                    .collect(Collectors.toList());
        }
    }
}