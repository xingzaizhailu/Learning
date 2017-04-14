IO.puts inspect "number = 13  Macro.to_string(quote do: 11 + number)"
number = 13
IO.puts inspect Macro.to_string(quote do: 11 + number)

IO.puts inspect "number = 13  Macro.to_string(quote do: 11 + number)"
IO.puts inspect Macro.to_string(quote do: 11 + unquote(number))

IO.puts inspect "fun = :hello  Macro.to_string(quote do: unquote(fun)(:world))"
fun = :hello
IO.puts inspect Macro.to_string(quote do: unquote(fun)(:world))

IO.puts inspect "inner = [3, 4, 5]  Macro.to_string(quote do: [1, 2, unquote(inner), 6])"
inner = [3, 4, 5]
IO.puts inspect Macro.to_string(quote do: [1, 2, unquote(inner), 6])

IO.puts inspect "inner = [3, 4, 5]  Macro.to_string(quote do: [1, 2, unquote_splicing(inner), 6])"
IO.puts inspect Macro.to_string(quote do: [1, 2, unquote_splicing(inner), 6])
