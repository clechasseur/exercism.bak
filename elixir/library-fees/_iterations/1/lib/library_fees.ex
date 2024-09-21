defmodule LibraryFees do
  def datetime_from_string(string) do
    NaiveDateTime.from_iso8601!(string)
  end

  def before_noon?(datetime) do
    noon = datetime |> NaiveDateTime.beginning_of_day() |> NaiveDateTime.add(12, :hour)
    NaiveDateTime.compare(datetime, noon) == :lt
  end

  def return_date(checkout_datetime) do
    allotted_time = if before_noon?(checkout_datetime), do: 28, else: 29
    checkout_datetime |> NaiveDateTime.add(allotted_time, :day) |> NaiveDateTime.to_date()
  end

  def days_late(planned_return_date, actual_return_datetime) do
    diff = Date.diff(NaiveDateTime.to_date(actual_return_datetime), planned_return_date)
    if diff > 0, do: diff, else: 0
  end

  def monday?(datetime) do
    (datetime |> NaiveDateTime.to_date() |> Date.day_of_week()) == 1
  end

  def calculate_late_fee(checkout, return, rate) do
    planned_return_date = return_date(datetime_from_string(checkout))
    return_date = datetime_from_string(return)
    rate = if monday?(return_date), do: rate * 0.5, else: rate
    floor(days_late(planned_return_date, return_date) * rate)
  end
end
