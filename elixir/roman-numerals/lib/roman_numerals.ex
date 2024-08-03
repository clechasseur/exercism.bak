defmodule RomanNumerals do
  @doc """
  Convert the number to a roman number.
  """
  @spec numeral(pos_integer) :: String.t()
  def numeral(number), do: do_numeral(number, [])

  # - Wait, it's all function clauses?
  # - Always has been. (*bang*)

  @roman_numerals [
    {1000, "M"},
    {900, "CM"},
    {500, "D"},
    {400, "CD"},
    {100, "C"},
    {90, "XC"},
    {50, "L"},
    {40, "XL"},
    {10, "X"},
    {9, "IX"},
    {5, "V"},
    {4, "IV"},
    {1, "I"},
  ]

  defp do_numeral(0, acc), do: acc |> Enum.reverse() |> Enum.join()
  for {arab, roman} <- @roman_numerals do
    defp do_numeral(n, acc) when n >= unquote(arab) do
      do_numeral(n - unquote(arab), [unquote(roman) | acc])
    end
  end
end
