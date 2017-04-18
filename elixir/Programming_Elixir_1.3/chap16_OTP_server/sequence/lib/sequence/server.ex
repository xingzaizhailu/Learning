defmodule LinearOpts.Sequence.Server do
  # 将OTP GenServer行为添加到当前模块
  use GenServer

  ##### 整理接口
  # 外部API
  def start_link(initial_number) do
    IO.puts "- start LinearOpts.Sequence.Server"

    GenServer.start_link(__MODULE__, initial_number, name: __MODULE__)
  end

  def next_number do
    GenServer.call __MODULE__, :next_number
  end

  def increment_number(delta) do
    GenServer.cast __MODULE__, {:increment_number, delta}
  end


  ##### 
  # Genserver 实现

  #    def init(stash_id) do
  #      IO.puts "at Sequence.Server.init"
  #      current_number = LinearOpts.Sequence.Stash.get_seq stash_id
  #      IO.puts "at sequence.server.init: current_number is #{current_number}"
  #      { :ok, {current_number, stash_id} }
  #    end

  # 整理接口前
  # -------------------------------------------------------------------
  # {:ok, pid} = GenServer.start_link(Sequence.Server, 100)
  # 命名进程：
  # {:ok, pid} = GenServer.start_link(Sequence.Server, 100, name: :seq)

  # 参数1：客户端传递给调用的信息
  # 参数2：客户端PID
  # 参数3：服务器状态
  #
  # GenServer.call(pid, :next_number)
  # GenServer.call(:seq, :next_number)
  def handle_call(:next_number, _from, current_number) do
    { :reply, current_number, current_number + 1}
  end

  # 元组：传入多个参数
  def  handle_call({:set_number, new_number}, _from, _current_number) do
    { :reply, new_number, new_number }
  end

  # 元组：返回多个值
  def handle_call({:factors, number}, _, _) do
    { :reply, { :factors_of, number, factors(number)}, [] }
  end


  # 参数1：调用参数
  # 参数2：当前状态
  #
  # GenServer.cast(pid, {:increment_number, 100})
  def handle_cast({:increment_number, delta}, current_number) do
    { :noreply, current_number + delta }
  end

  # 调试跟踪功能：将活动信息输出到控制台
  # {:ok, pid} = GenServer.start_link(Sequence.Server, 100, [debug: [:trace]])
  #
  # 在已有的服务上开启跟踪：:sys.trace pid, true
  #                         :sys.trace :seq, true

  # 让服务器做些简单的统计：
  # {:ok, pid} = GenServer.start_link(Sequence.Server, 100, [debug: [:trace, :statistics]])
  # :sys.statistics pid, :get
  #
  # reductions值用来衡量服务器工作量。用于进程调度。

  # :sys.get_status pid
  # 提供状态消息的默认格式。通过设定format_status还可以修改‘State’部分以返回更多该应用程序特有的信息。
  def format_status(_reason, [ _pdict, state ]) do
    [data: [{'State', "My current state is '#{inspect state}', and I'm happy"}]]
  end

  ## debug参数列表的元素，正是调用sys模块的函数名。

  ## 完整回调函数
  # init(start_arguments)
  # GenServer启动服务时被调用。其参数是start_link的第二个参数。
  # 成功返回：{:ok, state}
  # 失败返回：{:stop, reason}
  # 默认实现将服务器状态设置为传入参数的值
  #
  # handle_call(request, from, state)
  # 客户端使用GenServer.call(pid, request)调用。from包含客户端的PID和一个唯一标记。State是服务器的状态。
  # 函数执行成功返回：{:reply, result, new_state}
  # 默认返回一个:bad_call错误，停止服务器。所以你需要为每种服务器实现的调用请求类型实现handle_call
  #
  # handle_cast(request, state)
  # 以响应GenServer.Cast(pid, request)
  # 成功的响应是{:noreply, new_state}，也能返回{:stop, reason, new_state}
  # 默认返回:bad_cast错误，并停止服务器
  #
  # handle_info(info, state)
  # 处理call和cast以外的传入消息。
  #
  # terminate(reason, state)
  # 服务器既将终止时被调用。
  def terminate(_reason, {current_number, stash_id}) do
    IO.puts "current_number is #{current_number}"
    LinearOpts.Sequence.Stash.store_seq(stash_id, current_number)
  end
  #
  # code_change(from_version, state, extra)
  # 利用OTP替换正在运行的服务器，无序停止整个系统。回调code_change用于将旧版本的状态变成新的。
  #
  # format_status(reason, [pdict, state])
  # 定制服务器的状态显示。通常响应是[data: [{’State', state_info}]]

  # call 和cast通用
  # { :noreply, new_state [ , :hibernate | timeout ] }
  # { :stop, reason, new_state }    # 发出信号，表示服务器要终止了。
  #
  # 仅handle_call可以用
  # { :reply, response, new_state [ , :hibernate | timeout ] }
  # { :stop, reason, reply, new_state }   # 发送该信号，表示服务器要终止。



  def factors(number) do
    number
  end
end

