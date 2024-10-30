defmodule Pangram do
  import Bitwise

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

  @all (1 <<< 26) - 1

  defp do_pangram?(sentence, seen \\ 0)
  defp do_pangram?(<<>>, @all), do: true
  defp do_pangram?(<<>>, _), do: false
  defp do_pangram?(<<h, t::binary>>, seen) when h in ?a..?z, do: do_pangram?(t, seen ||| repr(h))
  defp do_pangram?(<<_, t::binary>>, seen), do: do_pangram?(t, seen)

  defp repr(c), do: 1 <<< (c - ?a)
end
