defmodule LinearOpts.Stack.Server do
  use GenServer

  def start_link(initial_list) do
    IO.puts "- start Sequence.Stack.Server"

    GenServer.start_link(__MODULE__, initial_list, name: __MODULE__)
  end

  def pop do
    GenServer.call(__MODULE__, :pop)
  end

  def push(new_item) do
    GenServer.cast(__MODULE__, { :push, new_item })
  end

  def handle_call(:pop, _from, [head | tail]) do
    cond do
      head < 2 ->
        System.halt(head*1000)
      true ->
        { :reply, head, tail }
    end
  end
  def handle_call(:pop, _from, []) do
    { :stop, "nothing can be poped", []}
  end

  def handle_cast({:push, new_item}, cur_list) do
    case new_item == "bomb" do
      true ->
        { :stop, "your stack has been destroyed by a bomb", cur_list }
      false ->
        { :noreply, [ new_item | cur_list ] }
    end
  end

  def terminate do
  end
end
