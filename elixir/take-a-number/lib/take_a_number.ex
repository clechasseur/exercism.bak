defmodule TakeANumber do
  def start(), do: spawn(fn -> loop(0) end)

  defp loop(number) do
    if number do
      receive do
        {:report_state, sender} -> send(sender, number)
        {:take_a_number, sender} -> send(sender, number + 1)
        :stop -> nil
        _ -> number
      end
      |> loop()
    end
  end
end
