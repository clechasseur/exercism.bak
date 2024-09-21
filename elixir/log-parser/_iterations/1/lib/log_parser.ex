defmodule LogParser do
  def valid_line?(line), do: String.match?(line, ~r/^\[(DEBUG|INFO|WARNING|ERROR)\]/)
  def split_line(line), do: String.split(line, ~r/<[~*=-]*>/)
  def remove_artifacts(line), do: String.replace(line, ~r/end-of-line\d+/i, "")
  def tag_with_user_name(line) do
    do_tag_with_user_name(line, Regex.run(~r/User[\s\t]+(\S+)/i, line))
  end

  defp do_tag_with_user_name(line, [_, username]), do: "[USER] #{username} #{line}"
  defp do_tag_with_user_name(line, nil), do: line
end
