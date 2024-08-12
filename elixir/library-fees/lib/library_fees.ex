defmodule LibraryFees do
  def datetime_from_string(string) do
    NaiveDateTime.from_iso8601!(string)
  end

  def before_noon?(datetime) do
    datetime.hour < 12
  end

  def return_date(checkout_datetime) do
    allotted_time = if before_noon?(checkout_datetime), do: 28, else: 29
    checkout_datetime |> NaiveDateTime.add(allotted_time, :day) |> NaiveDateTime.to_date()
  end

  def days_late(planned_return_date, actual_return_datetime) do
    actual_return_datetime
    |> NaiveDateTime.to_date()
    |> Date.diff(planned_return_date)
    |> max(0)
  end

  def monday?(datetime) do
    (datetime |> NaiveDateTime.to_date() |> Date.day_of_week()) == 1
  end

  def calculate_late_fee(checkout, return, rate) do
    planned_return_date = checkout |> datetime_from_string() |> return_date()
    return_date = return |> datetime_from_string()
    rate = if monday?(return_date), do: rate * 0.5, else: rate

    (days_late(planned_return_date, return_date) * rate) |> floor()
  end
end
