defmodule Unless do
  # iex> require Unless
  # iex> Unless.fun_unless true, do: IO.puts "this should never be printed"
  def fun_unless(clause, do: expression) do
    if(!clause, do: expression)
  end

  # iex > Unless.macro_unless true, do: IO.puts "this should never be printed"
  defmacro macro_unless(clause, do: expression) do
    quote do
      if(!unquote(clause), do: unquote(expression))
    end
  end
end
