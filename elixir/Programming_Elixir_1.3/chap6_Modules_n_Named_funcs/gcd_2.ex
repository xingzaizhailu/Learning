defmodule Gcd do
  # this is not correct when you call like gcd(12, 0)
  def gcd(x, 0), do: x
  def gcd(x, y), do: gcd(y, rem(x, y))
end
