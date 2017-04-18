defmodule LinearOpts.Sequence.SubSupervisor do
  use Supervisor

  def start_link(initial_number) do
    IO.puts "- LinearOpts.Sequence.SubSupervisor"

    result = {:ok, sup} = Supervisor.start_link(__MODULE__, [initial_number])
    start_workers(sup, initial_number)
    result
  end

  defp start_workers(sup, initial_number) do
    IO.puts "- LinearOpts.Sequence.SubSupervisor.start_workers"
    
    # 启动 Stash 工作进程
    {:ok, stash} = Supervisor.start_link(sup, worker(LinearOpts.Sequence.Server, [initial_number]))

    # 然后启动监视真实 sequence 服务器的子监视器
    # Supervisor.start_link(sup, supervisor(LinearOpts.Sequence.SubSupervisor, [stash]))

    import Supervisor.Spec, warn: false
    # children = [
    #   worker(LinearOpts.Sequence.Server, [initial_number])
    # ]
    # 
    # opts = [strategy: :one_for_one, name: LinearOpts.Sequence.SubSupervisor]
    # Supervisor.start_link(children, opts)
    # Wrong: Supervisor.start_link(worker(LinearOpts.Sequence.Server, [initial_number]), opts)
  end

  def init(_) do
    supervise [], strategy: :one_for_one
  end
end
