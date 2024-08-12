defmodule RPNCalculator do
  def calculate!(stack, operation), do: operation.(stack)

  def calculate_verbose(stack, operation) do
    {:ok, calculate!(stack, operation)}
  rescue
    e -> {:error, e.message}
  end

  def calculate(stack, operation) do
    calculate_verbose(stack, operation)
    |> do_calculate()
  end

  defp do_calculate({:ok, _} = result), do: result
  defp do_calculate({:error, _}), do: :error
end
