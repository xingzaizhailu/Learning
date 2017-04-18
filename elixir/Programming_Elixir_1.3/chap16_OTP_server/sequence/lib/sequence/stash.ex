defmodule LinearOpts.Sequence.Stash do
  use GenServer

  def start_link(initial_number) do
    IO.puts "- LinearOpts.Sequence.Stack"

    {:ok, _pid} = GenServer.start_link(__MODULE__, initial_number, name: __MODULE__)
  end

  def store_seq(pid, seq) do
    IO.puts "at store_seq: seq is #{seq}"
    GenServer.cast __MODULE__, {:store_seq, seq}
  end

  def get_seq(pid) do
    IO.puts "at get seq: stash pid is #{inspect pid}"

    GenServer.call __MODULE__, :get_seq    # correct
    # GenServer.call pid, :get_seq    # incorrect
  end


  def handle_call(:get_seq, _from, cur_seq) do
    { :reply, cur_seq, cur_seq }
  end

  def handle_cast({:store_seq, seq}, _cur_seq) do
    {:noreply, seq}
  end
end
