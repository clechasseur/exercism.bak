defmodule BasketballWebsite do
  def extract_from_path(data, path) do
    do_extract_from_path(data, String.split(path, "."))
  end

  # Technically, the first clause is not needed because `nil` implements Access,
  # but it seems like a nice short-circuit optimization... not sure if it's customary or not.
  defp do_extract_from_path(nil, _), do: nil
  defp do_extract_from_path(data, []), do: data
  defp do_extract_from_path(data, [k | t]) do
    do_extract_from_path(data[k], t)
  end

  def get_in_path(data, path) do
    get_in(data, String.split(path, "."))
  end
end
