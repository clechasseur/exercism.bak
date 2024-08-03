defmodule PaintByNumber do
  def palette_bit_size(0), do: 0
  def palette_bit_size(n) when n > 2, do: 1 + palette_bit_size(n / 2)
  def palette_bit_size(_), do: 1

  def empty_picture(), do: <<>>
  def test_picture(), do: <<0::2, 1::2, 2::2, 3::2>>

  def prepend_pixel(picture, color_count, pixel_color_index) do
    <<pixel_color_index::size(palette_bit_size(color_count)), picture::bitstring>>
  end

  def get_first_pixel(picture, color_count) do
    color_bit_count = palette_bit_size(color_count)
    case picture do
      <<first::size(color_bit_count), _::bitstring>> -> first
      _ -> nil
    end
  end

  def drop_first_pixel(picture, color_count) do
    color_bit_count = palette_bit_size(color_count)
    case picture do
      <<_::size(color_bit_count), rest::bitstring>> -> rest
      p -> p
    end
  end

  def concat_pictures(picture1, picture2) do
    <<picture1::bitstring, picture2::bitstring>>
  end
end
