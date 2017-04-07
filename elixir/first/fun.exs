defmodule Math do
 def sum(a,b) do
  a + b
 end
end

IO.puts Math.sum(1,2)


defmodule Recursion do
 def sum_list([head|tail],argument) do
  sum_list(tail,head+argument)
 end

 def sum_list([],argument) do
  argument
 end
end
IO.puts Recursion.sum_list([1,2,3],0)
