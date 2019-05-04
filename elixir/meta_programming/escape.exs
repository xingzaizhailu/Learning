IO.puts inspect "map = %{hello: :world}"
map = %{hello: :world}

IO.puts "quote do: map"
IO.puts inspect quote do: map

IO.puts "Macro.escape(map)"
IO.puts inspect Macro.escape(map)
