defmodule PhoneNumber do
  @doc """
  Remove formatting from a phone number if the given number is valid. Return an error otherwise.
  """
  @spec clean(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def clean(raw) do
    with {:ok, cleaned} <- without_punctuation(raw),
         <<area_code::binary-size(3), exchange_code::binary-size(3), _::binary>> = number <- normalized(cleaned),
         :ok <- validate_area_code(area_code),
         :ok <- validate_exchange_code(exchange_code) do
      {:ok, number}
    end
  end

  defp without_punctuation(number, cleaned \\ "")
  defp without_punctuation(<<>>, cleaned), do: {:ok, cleaned}
  defp without_punctuation(<<p::utf8, tail::binary>>, cleaned) when p in ~c[ .-()+], do: without_punctuation(tail, cleaned)
  defp without_punctuation(<<n::utf8, tail::binary>>, cleaned) when n in ?0..?9, do: without_punctuation(tail, [cleaned, n] |> List.to_string)
  defp without_punctuation(_, _), do: {:error, "must contain digits only"}

  defp normalized(<<"1", number::binary-size(10)>>), do: number
  defp normalized(<<_::binary-size(11)>>), do: {:error, "11 digits must start with 1"}
  defp normalized(<<number::binary-size(10)>>), do: number
  defp normalized(<<_::binary-size(11), _::binary>>), do: {:error, "must not be greater than 11 digits"}
  defp normalized(_), do: {:error, "must not be fewer than 10 digits"}

  defp validate_2_to_9(?0, part), do: {:error, part <> " cannot start with zero"}
  defp validate_2_to_9(?1, part), do: {:error, part <> " cannot start with one"}
  defp validate_2_to_9(_, _), do: :ok

  defp validate_area_code(<<n::utf8, _::binary>>), do: validate_2_to_9(n, "area code")
  defp validate_exchange_code(<<n::utf8, _::binary>>), do: validate_2_to_9(n, "exchange code")
end
