defmodule KitchenCalculator do
  def get_volume({_, volume}), do: volume

  def to_milliliter({:cup, volume}), do: {:milliliter, volume * 240.0}
  def to_milliliter({:fluid_ounce, volume}), do: {:milliliter, volume * 30.0}
  def to_milliliter({:teaspoon, volume}), do: {:milliliter, volume * 5.0}
  def to_milliliter({:tablespoon, volume}), do: {:milliliter, volume * 15.0}
  def to_milliliter({:milliliter, _} = volume_pair), do: volume_pair

  def from_milliliter({:milliliter, volume}, unit) do
    {:milliliter, proportion} = to_milliliter({unit, 1})
    {unit, volume / proportion}
  end

  def convert(from_volume_pair, to_unit) do
    from_milliliter(to_milliliter(from_volume_pair), to_unit)
  end
end
