defmodule Frequency do
  @doc """
  Count letter frequency in parallel.

  Returns a map of characters to frequencies.

  The number of worker processes to use can be set with 'workers'.
  """
  @spec frequency([String.t()], pos_integer) :: map
  def frequency(texts, workers) do
    Task.async_stream(texts, &do_frequency/1, max_concurrency: workers)
    |> Enum.reduce(%{}, &merge_freqs/2)
  end

  defp do_frequency(text) do
    text
    |> String.replace(~r/[^[:alpha:]]+/u, "")
    |> String.codepoints()
    |> Enum.frequencies_by(&String.downcase/1)
  end

  defp merge_freqs({:ok, freqs}, new_freqs) do
    Map.merge(freqs, new_freqs, fn _, c1, c2 -> c1 + c2 end)
  end
end
