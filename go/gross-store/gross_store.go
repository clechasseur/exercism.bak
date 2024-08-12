package gross

// Units stores the Gross Store unit measurements.
func Units() map[string]int {
	return map[string]int{
		"quarter_of_a_dozen": 3,
		"half_of_a_dozen":    6,
		"dozen":              12,
		"small_gross":        120,
		"gross":              144,
		"great_gross":        1728,
	}
}

// NewBill creates a new bill.
func NewBill() map[string]int {
	return map[string]int{}
}

// AddItem adds an item to customer bill.
func AddItem(bill, units map[string]int, item, unit string) (exists bool) {
	unitValue, exists := units[unit]
	if exists {
		bill[item] += unitValue
	}
	return
}

// RemoveItem removes an item from customer bill.
func RemoveItem(bill, units map[string]int, item, unit string) (exists bool) {
	unitValue, exists := units[unit]
	if !exists {
		return
	}

	newValue := bill[item] - unitValue
	if newValue < 0 {
		return false
	}

	if newValue > 0 {
		bill[item] = newValue
	} else {
		delete(bill, item)
	}
	return
}

// GetItem returns the quantity of an item that the customer has in his/her bill.
func GetItem(bill map[string]int, item string) (quantity int, exists bool) {
	quantity, exists = bill[item]
	return
}
