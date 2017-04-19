defmodule LinearOpts do
  use Application

  @doc """
    no stash version:
    应用程序被调用时，start函数被调用
    创建一个子服务器列表。使用worker函数为每个子服务器创建一个调用规范。
    调用Supervisor.start_link，给它传入进程的调用规范列表，并设置一些选项。这样就创建了一个应用程序监视其进程。
    应用程序监视器检查对所管理的子进程分别调用start_link函数来创建GenServer进程。
  """

  def start(_type, _args) do
    IO.puts "- start Application"

    # V1: no stash
    # no_stash()
    
    # V2: with stash
    stash()

    #{:ok, _pid} = LinearOpts.Supervisor.start_link
  end    

  def stash() do
    import Supervisor.Spec, warn: false
    children = [
      supervisor(LinearOpts.Sequence.Supervisor, [123])
    ]

    opts = [strategy: :rest_for_one, name: LinearOpts.Supervisor]
    Supervisor.start_link(children, opts)


  end

  def no_stash() do
    import Supervisor.Spec, warn: false

    children = [
      # 定义被监控的工作进程及子监视器
      # worker(Sequence.Worker, [arg1, arg2, arg3])
      # 一定要加[]!!!, list外面也再一层[]
      
      worker(LinearOpts.Sequence.Server, [123]),
      worker(LinearOpts.Stack.Server, [[5, "hello", :world]])
    ]

    opts = [strategy: :rest_for_one, name: LinearOpts.Supervisor]


    #Supervisor.start_link(children, opts)
    {:ok, _pid} = Supervisor.start_link(children, opts)
  end

end
