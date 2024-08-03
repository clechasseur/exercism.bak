defmodule NameBadge do
  def print(id, name, department) do
    "#{id_prefix(id)}#{name} - #{department_name(department)}"
  end

  defp id_prefix(id) do
    if id, do: "[#{id}] - ", else: ""
  end

  defp department_name(department) do
    if department, do: String.upcase(department), else: "OWNER"
  end
end
