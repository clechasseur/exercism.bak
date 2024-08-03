class Poker
  attr_reader :hands

  def initialize(hands_saa)
    @hands = hands_saa.map { Hand.new(_1) }
  end

  def best_hand
    bh = hands.max
    hands.filter { _1 == bh }.map(&:cards_sa)
  end

  private

  CARDS = %w[2 3 4 5 6 7 8 9 10 J Q K A]
  RANKS = %i[high_card pair two_pairs three_of_a_kind straight flush full_house four_of_a_kind straight_flush]

  class Card
    include Comparable

    attr_reader :value, :suit

    def initialize(card_s)
      @value = card_s[0..-2]
      @suit = card_s[-1]
    end

    def <=>(other)
      CARDS.index(value) <=> CARDS.index(other.value)
    end
  end

  class Hand
    include Comparable

    attr_reader :cards_sa, :cards, :rank, :contributing_cards, :non_contributing_cards

    def initialize(cards_sa)
      @cards_sa = cards_sa
      @cards = cards_sa.map { Card.new(_1) }.sort

      if straight?
        if flush?
          @rank = :straight_flush
        else
          @rank = :straight
        end
        
        if low_straight?
          @contributing_cards = [cards.last] + cards[0..-2]
        end
      elsif flush?
        @rank = :flush
      elsif full_house?
        @rank = :full_house
        if cards[1] == cards[2]
          @contributing_cards = cards[0..2]
          @non_contributing_cards = cards[3..4]
        else
          @contributing_cards = cards[2..4]
          @non_contributing_cards = cards[0..1]
        end
      else
        chunks = cards.chunk(&:value).sort do |c1, c2|
          cmp = c1.last.size <=> c2.last.size
          cmp = c1.first <=> c2.first if cmp.zero?
          cmp
        end.map(&:last)

        case chunks.last.size
        when 4
          @rank = :four_of_a_kind
        when 3
          @rank = :three_of_a_kind
        when 2
          if chunks.filter { _1.size == 2 }.size == 2
            @rank = :two_pairs
            @contributing_cards = chunks[-2..-1].flatten
            @non_contributing_cards = chunks[0]
          else
            @rank = :pair
          end
        else
          @rank = :high_card
        end

        @contributing_cards ||= chunks.last
        @non_contributing_cards ||= chunks[0..-2].flatten
      end

      @contributing_cards ||= cards
      @non_contributing_cards ||= []
    end

    def <=>(other)
      cmp = RANKS.index(rank) <=> RANKS.index(other.rank)
      if cmp.zero?
        contributing_cards.reverse.zip(other.contributing_cards.reverse).each do |c1, c2|
          cmp = c1 <=> c2 if cmp.zero?
        end
      end
      if cmp.zero?
        non_contributing_cards.reverse.zip(other.non_contributing_cards.reverse).each do |c1, c2|
          cmp = c1 <=> c2 if cmp.zero?
        end
      end
      cmp
    end

    private

    def flush?
      cards.all? { |c| c.suit == cards.first.suit }
    end

    def straight?
      cards.drop(1).each_with_index.all? do |c, i|
        CARDS.index(cards[i].value) + 1 == CARDS.index(c.value) ||
        (i == 3 && cards[i].value == "5" && c.value == "A")
      end
    end

    def low_straight?
      straight? && cards[3].value == "5" && cards[4].value == "A"
    end

    def full_house?
      (cards[0] == cards[1] && cards[2] == cards[3] && cards[3] == cards[4] && cards[1] != cards[2]) ||
      (cards[0] == cards[1] && cards[1] == cards[2] && cards[3] == cards[4] && cards[2] != cards[3])
    end
  end
end
