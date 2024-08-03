defmodule Username do
  def sanitize(username), do: Enum.flat_map(username, &sanitize_one/1)

  defp sanitize_one(codepoint) do
    case codepoint do
      ?ä -> ~c"ae"
      ?ö -> ~c"oe"
      ?ü -> ~c"ue"
      ?ß -> ~c"ss"
      valid when valid in ?a..?z or valid == ?_ -> [valid]
      _ -> []
    end
  end
end
