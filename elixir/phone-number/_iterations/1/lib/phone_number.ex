defmodule PhoneNumber do
  @doc """
  Remove formatting from a phone number if the given number is valid. Return an error otherwise.
  """
  @spec clean(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def clean(raw) do
    with {:ok, cleaned} <- do_cleanup(raw, ""),
         {:ok, tail} <- do_validate_size(cleaned),
         {:ok, tail, number} <- do_area_code(tail),
         {:ok, tail, number} <- do_exchange_code(tail, number),
         {:ok, number} <- do_subscriber_number(tail, number) do
      {:ok, number}
    end
  end

  defp do_cleanup(<<>>, cleaned), do: {:ok, cleaned}
  defp do_cleanup(<<p::utf8, tail::binary>>, cleaned) when p in ~c[ .-()+], do: do_cleanup(tail, cleaned)
  defp do_cleanup(<<n::utf8, tail::binary>>, cleaned) when n in ?0..?9, do: do_cleanup(tail, [cleaned, n] |> List.to_string)
  defp do_cleanup(_, _), do: {:error, "must contain digits only"}

  defp do_validate_size(<<"1", tail::binary-size(10)>>), do: {:ok, tail}
  defp do_validate_size(<<_::binary-size(11)>>), do: {:error, "11 digits must start with 1"}
  defp do_validate_size(<<tail::binary-size(10)>>), do: {:ok, tail}
  defp do_validate_size(tail) do
    if String.length(tail) < 10 do
      {:error, "must not be fewer than 10 digits"}
    else
      {:error, "must not be greater than 11 digits"}
    end
  end

  defp do_2_to_9(<<"0", _::binary>>, _, part), do: {:error, part <> " cannot start with zero"}
  defp do_2_to_9(<<"1", _::binary>>, _, part), do: {:error, part <> " cannot start with one"}
  defp do_2_to_9(<<n::utf8, tail::binary>>, number, _), do: {:ok, tail, [number, n] |> List.to_string}

  defp do_code(tail, number, part) do
    with {:ok, tail, number} <- do_2_to_9(tail, number, part),
         <<d1::utf8, d2::utf8, tail::binary>> <- tail do
      {:ok, tail, [number, d1, d2] |> List.to_string}
    end
  end

  defp do_area_code(tail), do: do_code(tail, "", "area code")
  defp do_exchange_code(tail, number), do: do_code(tail, number, "exchange code")

  defp do_subscriber_number(tail, number) do
    with <<d0::utf8, d1::utf8, d2::utf8, d3::utf8>> <- tail do
      {:ok, [number, d0, d1, d2, d3] |> List.to_string}
    end
  end
end
