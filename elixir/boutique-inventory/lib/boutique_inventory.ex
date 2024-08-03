defmodule BoutiqueInventory do
  def sort_by_price(inventory) do
    inventory |> Enum.sort_by(&(&1[:price]))
  end

  def with_missing_price(inventory) do
    inventory |> Enum.filter(&(!&1[:price]))
  end

  def update_names(inventory, old_word, new_word) do
    inventory |> Enum.map(fn %{name: name} = item ->
      %{item | name: String.replace(name, old_word, new_word)}
    end)
  end

  def increase_quantity(%{quantity_by_size: qbs} = item, count) do
    qbs = Map.new(qbs, fn {size, cur_count} -> {size, cur_count + count} end)
    %{item | quantity_by_size: qbs}
  end

  def total_quantity(%{quantity_by_size: qbs}) do
    qbs |> Map.values() |> Enum.reduce(0, &+/2)
  end
end
