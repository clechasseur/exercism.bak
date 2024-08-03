// Package weather includes helpers to deliver weather forecasts.
package weather

// CurrentCondition stores the last forecast weather condition.
var CurrentCondition string

// CurrentLocation stores the location of the last weather forecast.
var CurrentLocation string

// Forecast stores information about the weather condition forecast for
// a specific location and returns a textual representation of the forecast.
func Forecast(city, condition string) string {
	CurrentLocation, CurrentCondition = city, condition
	return CurrentLocation + " - current weather condition: " + CurrentCondition
}
