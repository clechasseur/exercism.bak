defmodule Pangram do
  @doc """
  Determines if a word or sentence is a pangram.
  A pangram is a sentence using every letter of the alphabet at least once.

  Returns a boolean.

    ## Examples

      iex> Pangram.pangram?("the quick brown fox jumps over the lazy dog")
      true

  """
  @spec pangram?(String.t()) :: boolean
  def pangram?(sentence), do: do_pangram?(String.downcase(sentence))

  defp do_pangram?(sentence, seen \\ MapSet.new())
  defp do_pangram?(<<>>, seen), do: MapSet.size(seen) == 26
  defp do_pangram?(<<head, tail::binary>>, seen) when head in ?a..?z, do: do_pangram?(tail, MapSet.put(seen, head))
  defp do_pangram?(<<_, tail::binary>>, seen), do: do_pangram?(tail, seen)
end
