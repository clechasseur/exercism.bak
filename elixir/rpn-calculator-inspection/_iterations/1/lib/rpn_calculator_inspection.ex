defmodule RPNCalculatorInspection do
  def start_reliability_check(calculator, input) do
    %{input: input, pid: spawn_link(fn -> calculator.(input) end)}
  end

  def await_reliability_check_result(%{pid: pid, input: input}, results) do
    result = receive do
      {:EXIT, ^pid, :normal} -> :ok
      {:EXIT, ^pid, _} -> :error
    after
      100 -> :timeout
    end

    Map.put(results, input, result)
  end

  def reliability_check(calculator, inputs) do
    old_flag = Process.flag(:trap_exit, true)

    results = Enum.map(inputs, fn input -> start_reliability_check(calculator, input) end)
              |> Enum.reduce(%{}, &await_reliability_check_result/2)

    Process.flag(:trap_exit, old_flag)
    results
  end

  def correctness_check(calculator, inputs) do
    Enum.map(inputs, fn input ->
      Task.async(fn -> calculator.(input) end)
    end)
    |> Enum.map(fn task -> Task.await(task, 100) end)
  end
end
