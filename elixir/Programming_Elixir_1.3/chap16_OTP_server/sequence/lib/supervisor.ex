defmodule LinearOpts.Supervisor do
  use Supervisor

  def start_link do
    IO.puts "- start LinearOpts.Supervisor"
    
    # import Supervisor.Spec, warn: false
    # children = [
    #   supervisor(LinearOpts.Sequence.Supervisor, [123])
    # ]

    # opts = [strategy: :one_for_one, name: LinearOpts.Supervisor]
    # Supervisor.start_link(children, opts)

  end

  def start_link(initial_number) do

  end
end
