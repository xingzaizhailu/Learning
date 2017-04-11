defmodule TRYMIXNOTP.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    children = [
      # start a process using the following call:
      # TRYMIXOTP.Registry.start_link(TRYMIXNOTP.Registry)
      #                                         |
      #                                         --> name of the process
      worker(TRYMIXNOTP.Registry, [TRYMIXNOTP.Registry])
    ]

    Supervisor(children, strategy: :one_for_one)
  end
end

