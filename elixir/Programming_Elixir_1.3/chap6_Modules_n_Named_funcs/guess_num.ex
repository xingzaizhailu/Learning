defmodule GuessNum do
  def guess(actual \\ 2, range \\ 1..1000)
  def guess(actual, range) do 
    a..b = range
    mid = compare(range)
    cond do
      actual-mid == 0 ->
        IO.inspect mid 
      actual-mid < 0 ->
        IO.puts "Is it #{mid}?"
        guess(actual, a..mid-1)
      actual-mid > 0 ->
        IO.puts "Is it #{mid}?"
        guess(actual, mid+1..b)
    end
  end

  def compare(range) do
    a..b = range
    div(a + b, 2)
  end
end
