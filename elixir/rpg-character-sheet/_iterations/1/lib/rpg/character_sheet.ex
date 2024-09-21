defmodule RPG.CharacterSheet do
  def welcome() do
    IO.puts("Welcome! Let's fill out your character sheet together.")
  end

  def ask_name(), do: ask("name")
  def ask_class(), do: ask("class")
  def ask_level(), do: ask("level") |> String.to_integer()

  def run() do
    welcome()

    %{
      name: ask_name(),
      class: ask_class(),
      level: ask_level()
    }
    |> IO.inspect(label: "Your character")
  end

  defp ask(quest) do
    # What is the airspeed velocity of an unladen swallow?
    IO.gets("What is your character's #{quest}?\n")
    |> String.trim()
  end
end
