defmodule LinearOpts.Sequence.Supervisor do
  use Supervisor

  def start_link(initial_number) do
    IO.puts "- LinearOpts.Sequence.Supervisor"
      
    # result = {:ok, sup} = Supervisor.start_link(__MODULE__, [initial_number], strategy: :one_for_one)
    # result

    children = [
      worker(LinearOpts.Sequence.Stash, [initial_number])
    ]

    opts = [strategy: :one_for_one, name: LinearOpts.Sequence.Supervisor]
    {:ok, stash} = Supervisor.start_link(children, opts)

    start_workers(initial_number)
  end

  def start_workers(initial_number) do
    IO.puts "-- Start workers"

    children = [
      # worker(LinearOpts.Sequence.Server, [LinearOpts.Sequence.Stash.get_seq])
      worker(LinearOpts.Sequence.Server, [initial_number])
    ]

    opts = [strategy: :one_for_one]
    Supervisor.start_link(children, opts)
  end
end
