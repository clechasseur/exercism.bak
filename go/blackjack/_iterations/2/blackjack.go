package blackjack

// ParseCard returns the integer value of a card following blackjack ruleset.
func ParseCard(card string) (value int) {
	switch card {
	case "two":
		value = 2
	case "three":
		value = 3
	case "four":
		value = 4
	case "five":
		value = 5
	case "six":
		value = 6
	case "seven":
		value = 7
	case "eight":
		value = 8
	case "nine":
		value = 9
	case "ten", "jack", "queen", "king":
		value = 10
	case "ace":
		value = 11
	default:
		value = 0
	}

	return
}

// FirstTurn returns the decision for the first turn, given two cards of the
// player and one card of the dealer.
func FirstTurn(card1, card2, dealerCard string) (decision string) {
	total := ParseCard(card1) + ParseCard(card2)
	dealerValue := 0
	getDealerValue := func() int {
		if dealerValue == 0 {
			dealerValue = ParseCard(dealerCard)
		}
		return dealerValue
	}

	switch {
	case total == 22:
		decision = "P"
	case total == 21 && getDealerValue() < 10:
		decision = "W"
	case total >= 17, total >= 12 && getDealerValue() < 7:
		decision = "S"
	default:
		decision = "H"
	}

	return
}
