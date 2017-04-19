defmodule TRYMIXNOTP.Registry do
  use GenServer

  ## Client API

  @doc """
  Starts the registry with the given `name`.
  """
  def start_link(name) do
    # 1. Pass the name to Genserver's init
    GenServer.start_link(__MODULE__, :ok, name: name)
  end

  @doc """
  Looks up the bucket pid for `name` stored in `server`.

  Returns `{:ok, pid}` if the bucket exists, `:error` otherwise
  """
  def lookup(server, name) when is_atom(server) do
    #GenServer.call(server, {:lookup, name})
    
    # 2. Lookup is now done directly in ETS, without accessing the server
    case :ets.lookup(server, name) do
      [{^name, pid}] -> {:ok, pid}
      [] -> :error
    end
  end

  @doc """
  Ensure there is a bucket associated to the given `name` in `server`.
  """
  def create(server, name) do
    # GenServer.cast(server, {:create, name})
    GenServer.call(server, {:create, name}) 
  end

  @doc """
  Stops the registry.
  """
  def stop(server) do
    GenServer.stop(server)
  end


  ### Server Callbacks
  ###
  def init(table) do
    # initial -> def init(:ok) do
    # names = %{}
    
    # 3. We have replaced the names map by the ETS table.
    names = :ets.new(table, [:named_table, read_concurrency: true])
    refs  = %{}
    {:ok, {names, refs}}
  end

  # 4. The previous handle_call callback for lookup
  #def handle_call({:lookup, name}, _from, {names, _} = state) do
  #  {:reply, Map.fetch(names, name), state}
  #end

  # change to ETS
  #  def handle_cast({:create, name}, {names, refs}) do
  #    if Map.has_key?(names, name) do
  #      {:noreply, {names, refs}}
  #    else
  #      # {:ok, bucket} = TRYMIXNOTP.Bucket.start_link
  #      # {:ok, pid} = TRYMIXNOTP.Bucket.start_link
  #      {:ok, pid} = TRYMIXNOTP.Bucket.Supervisor.start_bucket
  #      ref = Process.monitor(pid)
  #      refs = Map.put(refs, ref, name)
  #      names = Map.put(names, name, pid)
  #      {:noreply, {names, refs}}
  #    end
  #  end

  #def handle_cast({:create, name}, {names, refs}) do
  def handle_call({:create, name}, _from, {names, refs}) do
    # 5. Read and write to the ETS table instead of the map.
    case lookup(names, name) do
      #{:ok, _pid} ->
      {:ok, pid} ->
        # {:noreply, {names, refs}}
        {:reply, pid, {names, refs}}
      :error ->
      {:ok, pid} = TRYMIXNOTP.Bucket.Supervisor.start_bucket
      ref = Process.monitor(pid)
      refs = Map.put(refs, ref, name)
      :ets.insert(names, {name, pid})
      # {:noreply, {names, refs}}
      {:reply, pid, {names, refs}}
    end
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, {names, refs}) do
    {name, refs} = Map.pop(refs, ref)
    
    # change to ETS
    #names = Map.delete(names, name)
    # 6. Delete from the ETS table instead of the map
    :ets.delete(names, name)
    {:noreply, {names, refs}}
  end
  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
