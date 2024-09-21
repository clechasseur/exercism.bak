defmodule Frequency do
  @doc """
  Count letter frequency in parallel.

  Returns a map of characters to frequencies.

  The number of worker processes to use can be set with 'workers'.
  """
  @spec frequency([String.t()], pos_integer) :: map
  def frequency(texts, workers) do
    manager = start_manager(workers)

    Enum.each(texts, fn text -> send(manager, {:compute_frequencies, text}) end)
    send(manager, {:done, self()})

    receive do
      {:ok, freqs} -> freqs
      msg -> raise "unexpected message received from manager: #{msg}"
    end
  end

  # This producer/consumer implementation was inspired by:
  # https://benjamintan.io/blog/2014/10/11/producer-consumer-problem-in-elixir/

  # Manager

  defp start_manager(workers) do
    spawn(fn ->
      consumers = Enum.map(1..workers, fn _ -> start_consumer(self()) end)
      manager_loop(%{workers: workers, pending: [], consumers: consumers, freqs: %{}})
    end)
  end

  defp manager_loop(%{pending: [:finish | tail_t], consumers: [head_c | tail_c]} = state) do
    send(head_c, :done)
    manager_loop(%{state | pending: tail_t, consumers: tail_c})
  end

  defp manager_loop(%{pending: [{:done, caller}], freqs: freqs}), do: send(caller, {:ok, freqs})

  defp manager_loop(%{pending: [head_t | tail_t], consumers: [head_c | tail_c]} = state) do
    send(head_c, {:compute_frequencies, head_t})
    manager_loop(%{state | pending: tail_t, consumers: tail_c})
  end

  defp manager_loop(%{workers: workers, pending: pending, consumers: consumers, freqs: freqs} = state) do
    receive do
      {:compute_frequencies, text} ->
        manager_loop(%{state | pending: [text | pending]})

      {:frequencies, consumer, new_freqs} ->
        merged_freqs = Map.merge(freqs, new_freqs, fn _, t1, t2 -> t1 + t2 end)
        manager_loop(%{state | consumers: [consumer | consumers], freqs: merged_freqs})

      {:done, caller} ->
        finish_signals = Enum.map(1..workers, fn _ -> :finish end)
        manager_loop(%{state | pending: pending ++ finish_signals ++ [{:done, caller}]})
    end
  end

  # Consumer

  defp start_consumer(manager_pid) do
    spawn(fn -> consumer_loop(manager_pid) end)
  end

  defp consumer_loop(manager_pid) do
    receive do
      {:compute_frequencies, text} ->
        send(manager_pid, {:frequencies, self(), compute_frequencies(text)})
        consumer_loop(manager_pid)

      :done -> nil
    end
  end

  defp compute_frequencies(text) do
    text
    |> String.codepoints()
    |> Enum.reduce(%{}, fn cp, acc ->
      if cp =~ ~r/\p{L}/u do
        Map.update(acc, String.downcase(cp), 1, fn count -> count + 1 end)
      else
        acc
      end
    end)
  end
end
