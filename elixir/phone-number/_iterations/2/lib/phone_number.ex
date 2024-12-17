defmodule PhoneNumber do
  @doc """
  Remove formatting from a phone number if the given number is valid. Return an error otherwise.
  """
  @spec clean(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def clean(raw) do
    with {:ok, cleaned} <- do_cleanup(raw),
         {:ok, number} <- do_validate_length_and_remove_country_code(cleaned),
         <<area_code::binary-size(3), exchange_code::binary-size(3), _::binary>> <- number,
         {:ok} <- do_validate_area_code(area_code),
         {:ok} <- do_validate_exchange_code(exchange_code) do
      {:ok, number}
    end
  end

  defp do_cleanup(number, cleaned \\ "")
  defp do_cleanup(<<>>, cleaned), do: {:ok, cleaned}
  defp do_cleanup(<<p::utf8, tail::binary>>, cleaned) when p in ~c[ .-()+], do: do_cleanup(tail, cleaned)
  defp do_cleanup(<<n::utf8, tail::binary>>, cleaned) when n in ?0..?9, do: do_cleanup(tail, [cleaned, n] |> List.to_string)
  defp do_cleanup(_, _), do: {:error, "must contain digits only"}

  defp do_validate_length_and_remove_country_code(<<"1", number::binary-size(10)>>), do: {:ok, number}
  defp do_validate_length_and_remove_country_code(<<_::binary-size(11)>>), do: {:error, "11 digits must start with 1"}
  defp do_validate_length_and_remove_country_code(<<number::binary-size(10)>>), do: {:ok, number}
  defp do_validate_length_and_remove_country_code(<<_::binary-size(11), _::binary>>), do: {:error, "must not be greater than 11 digits"}
  defp do_validate_length_and_remove_country_code(_), do: {:error, "must not be fewer than 10 digits"}

  defp do_validate_2_to_9(?0, part), do: {:error, part <> " cannot start with zero"}
  defp do_validate_2_to_9(?1, part), do: {:error, part <> " cannot start with one"}
  defp do_validate_2_to_9(_, _), do: {:ok}

  defp do_validate_area_code(<<n::utf8, _::binary>>), do: do_validate_2_to_9(n, "area code")
  defp do_validate_exchange_code(<<n::utf8, _::binary>>), do: do_validate_2_to_9(n, "exchange code")
end
