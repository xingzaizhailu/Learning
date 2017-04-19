IO.puts "\ninspect quote do: sum(1, 2, 3) =>"
IO.puts inspect quote do: sum(1, 2, 3) 

IO.puts "\ninspect quote do: 1 + 2 =>"
IO.puts inspect quote do: 1 + 2

IO.puts "\ninspect quote do: %{1 => 2} =>"
IO.puts inspect quote do: %{1 => 2}

IO.puts "\ninspect quote do: x => "
IO.puts inspect quote do: x

IO.puts "\ninspect quote do: sum(1, 2 + 3, 4) =>"
IO.puts inspect quote do: sum(1, 2 + 3, 4)

IO.puts "\ninspect Macro.to_string(quote do: sum(1, 2 + 3, 4)) =>"
IO.puts inspect Macro.to_string(quote do: sum(1, 2 + 3, 4))

IO.puts "\ninspect quote do: :atom => "
IO.puts inspect quote do: atom

IO.puts "\ninspect quote do: 1.0 => "
IO.puts inspect quote do: 1.0

IO.puts "\ninspect quote do: [1, 2] => "
IO.puts inspect quote do: [1, 2]

IO.puts "\ninspect 'strings' => "
IO.puts inspect quote do: "string"

IO.puts "\ninspect {key, value} => "
IO.puts inspect quote do: {key, value}

#IO.puts "\ninspect => "
#IO.puts inspect quote do:
