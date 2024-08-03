package booking

import "time"

// Schedule returns a time.Time from a string containing a date.
func Schedule(date string) (value time.Time) {
	return parse("1/2/2006 15:04:05", date)
}

// HasPassed returns whether a date has passed.
func HasPassed(date string) bool {
	return parse("January 2, 2006 15:04:05", date).Before(time.Now())
}

// IsAfternoonAppointment returns whether a time is in the afternoon.
func IsAfternoonAppointment(date string) bool {
	hour := parse("Monday, January 2, 2006 15:04:05", date).Hour()
	return hour >= 12 && hour < 18
}

// Description returns a formatted string of the appointment time.
func Description(date string) string {
	return Schedule(date).Format("You have an appointment on Monday, January 2, 2006, at 15:04.")
}

// AnniversaryDate returns a Time with this year's anniversary.
func AnniversaryDate() time.Time {
	year := time.Now().Year()
	return time.Date(year, time.September, 15, 0, 0, 0, 0, time.UTC)
}

func parse(layout, date string) time.Time {
	value, err := time.Parse(layout, date)
	if err != nil {
		return time.Time{}
	}
	return value
}
