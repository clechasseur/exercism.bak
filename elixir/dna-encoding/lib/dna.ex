defmodule DNA do
  import Bitwise

  @nucleic_acids [?\s, ?A, ?C, ?G, ?T]

  def encode_nucleotide(code_point) do
    do_encode_nucleotide(@nucleic_acids, code_point, -1)
  end

  defp do_encode_nucleotide([target | _], target, offset), do: 1 <<< offset
  defp do_encode_nucleotide([_ | t], target, offset) do
    do_encode_nucleotide(t, target, offset + 1)
  end

  def decode_nucleotide(encoded_code) do
    do_decode_nucleotide(@nucleic_acids, encoded_code, -1)
  end

  defp do_decode_nucleotide([h | _], target, offset) when target == (1 <<< offset), do: h
  defp do_decode_nucleotide([_ | t], target, offset) do
    do_decode_nucleotide(t, target, offset + 1)
  end

  def encode(dna), do: do_encode(dna, <<>>)

  defp do_encode([], acc), do: acc
  defp do_encode([h | t], acc), do: do_encode(t, <<acc::bitstring, encode_nucleotide(h)::4>>)

  def decode(dna), do: do_decode(dna, [])

  defp do_decode(<<>>, acc), do: do_reverse(acc, [])
  defp do_decode(<<h::4, t::bitstring>>, acc), do: do_decode(t, [decode_nucleotide(h) | acc])

  defp do_reverse([], acc), do: acc
  defp do_reverse([h | t], acc), do: do_reverse(t, [h | acc])
end
