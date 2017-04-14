defmodule TRYMIXNOTP.Supervisor do
  use Supervisor

  # Return: {:ok, pid}
  def start_link(opts \\ []) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

  # Return: value
  def init(:ok) do
    children = [
      # start a process using the following call:
      # TRYMIXOTP.Registry.start_link(TRYMIXNOTP.Registry)
      #                                         |
      #                                         --> name of the process
      worker(TRYMIXNOTP.Registry, [TRYMIXNOTP.Registry]),

      # Added a supervisor as a child, starting it with no arguments.
      supervisor(TRYMIXNOTP.Bucket.Supervisor, [])
      #
    ]

    # supervise(children, options)
    supervise(children, strategy: :rest_for_one)
    # :one_for_one -> If a child process terminates, only that process is restarted. This strategy requires the supervisor specification to contain only one child. 
    # :rest_for_one -> when a child process crashes, the supervisor will only kill and restart child processes which were started after the crashed child.
    # :one_for_all strategy will kill and restart all of its children processes whenever any one of them dies.
    # Similar to :one_for_one but suits better when dynamically attaching children.
    # Many functions in this module behave slightly differently when this strategy is used
  end
end

