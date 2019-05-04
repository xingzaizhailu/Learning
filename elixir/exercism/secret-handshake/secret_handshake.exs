defmodule SecretHandshake do
  @doc """
  Determine the actions of a secret handshake based on the binary
  representation of the given `code`.

  If the following bits are set, include the corresponding action in your list
  of commands, in order from lowest to highest.

  1 = wink
  10 = double blink
  100 = close your eyes
  1000 = jump

  10000 = Reverse the order of the operations in the secret handshake
  """
  @spec commands(code :: integer) :: list(String.t())
  def commands(code) do
    acts = ["wink", "double blink", "close your eyes", "jump"]
    code = 
      Integer.digits(code, 2)
      |> Enum.take(-5)
      |> Enum.reverse()
    IO.puts "code: #{code |> inspect}"
    IO.puts "acts: #{acts |> inspect}"
    gen_acts([], code, acts)
  end

  def gen_acts(result, [], _) do
    count = Enum.count(result)
    cond do
      count > 1 ->
        result
        |> Enum.reverse
      true ->
        result
    end
  end
  def gen_acts(result, _, []) do
    result
  end
  def gen_acts(result, [1| c_t] = code, [a_h| a_t] = acts) do
    [a_h | result]
    |> gen_acts(c_t, a_t)
  end
  def gen_acts(result, [0| c_t] = code, [_| a_t] = acts) do
    result
    |> gen_acts(c_t, a_t)
  end
end

