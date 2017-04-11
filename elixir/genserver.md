## [Genserver](http://elixir-lang.org/getting-started/mix-otp/genserver.html)
Named registry:

    iex> Agent.start_link(fn -> %{} end, name: :shopping)
    {:ok, #PID<0.*.0>}
    iex> TRYMIXNOTP.Bucket.put(:shopping, "milk", 1)
    :ok
    iex> TRYMIXNOTP.Bucket.get(:shopping, "milk")
    1

**However**, this is terrible! **We should never convert user input to atoms**, because atoms are not garbage collected, while process names in Elixir must be atoms.   
Thus, we will create our own `registry processs` that holds a map that associates the buckt name to the bucket process.
The registry needs to guarantee that the dictionary is always up to date and to monitor each bucket. So we use GenServer.

### Our first GenServer
A GenServer is implemented in two parts: the client API and the server callbacks. You can either combine both parts in a single module or you can separate them.   
Here we'll use a single module for both the server callbacks and the client API.   
Create a new file at `lib/try_mix_n_otp/registry.ex` with the following contents:

```elixir
    defmodule KV.Registry do
      use GenServer

      ## Client API

      @doc """
        Starts the registry.
          """
            def start_link do
                GenServer.start_link(__MODULE__, :ok, [])
        end

      @doc """
      Looks up the bucket pid for `name` stored in `server`.

      Returns `{:ok, pid}` if the bucket exists, `:error` otherwise.
      """
      def lookup(server, name) do
        GenServer.call(server, {:lookup, name})
      end

      @doc """
      Ensures there is a bucket associated to the given `name` in `server`.
      """
      def create(server, name) do
        GenServer.cast(server, {:create, name})
      end

      ## Server Callbacks

      def init(:ok) do
        {:ok, %{}}
      end

      def handle_call({:lookup, name}, _from, names) do
        {:reply, Map.fetch(names, name), names}
      end

      def handle_cast({:create, name}, names) do
        if Map.has_key?(names, name) do
          {:noreply, names}
        else
          {:ok, bucket} = KV.Bucket.start_link
          {:noreply, Map.put(names, name, bucket)}
        end
      end
    end
```
There are two types of requests you can send to GenServer: `call`s and `cast`s. Calls are synchronous and the server **must** send a response back to such requests. Casts are asynchronous and the server won't send a response back.   
Requests are often specified as tuples.  

On the server side. The `init/1` callback, that receives the argument given to `Genserver.start_link/3` and returns `{:ok, state}`, where state is a new map.
For `call/2` requests, we implement a `handle_call/3` callback that receives the `request`, the process from which we received the request (`_from`), and the current server state (`names`). The `handle_call/3` callback returns a tuple in the format `{:reply, reply, new_state}`.   
For `cast/2` requests, we implement a `handle_cast/2` callback that receives the request and the current server state (`names`). The `handle_cast/2` callback returns a tuple in the format `{:noreply, new_state}`.   
Explore other tuple formats and callbacks(`terminate/2` and `code_change/3`) at [full GenServer documentation](https://hexdocs.pm/elixir/GenServer.html).

### Testing a GenServer
Create a file at `test/try_mix_n_otp/registry_test.exs` with the following:

``` elixir
    def module TRYMIXNOTP.RegistryTest do
      use ExUnit.Case, async: true

        setup do
          {:ok, registry} = TRYMIXNOTP.Registry.start_link
          {:ok, registry: registry}
        end
            
        test "spawns buckets", %{registry: registry} do
          assert TRYMIXNOTP.Registry.lookup(registry, "shopping") == :error
                  
          TRYMIXNOTP.Registry.create(registry, "shopping")
          assert {:ok, bucket} = TRYMIXNOTP.Registry.lookup(registry, "shopping")
                          
          TRYMIXNOTP.Bucket.put(bucket, "milk", 1)
          assert TRYMIXNOTP.Bucket.get(bucket, "milk") == 1
      end
    end
```

We don’t need to explicitly shut down the registry because it will receive a :shutdown signal when our test finishes. While this solution is ok for tests, if there is a need to stop a GenServer as part of the application logic, one can use the GenServer.stop/1 function:  

``` elixir
    ## Client API
    @doc """
    Stops the registry
    """
    def stop(server) do
      GenServer.stop(server)
    end
```

### The need for monitoring
Our registry is almost complete. The only remaining issue is that the registry may become stale if a bucket stops or crashes. Let’s add a test to TRYMIXNOTP.RegistryTest that exposes this bug:

``` elixir
    test "removes buckets on exit", %{registry: registry} do
      TRYMIXNOTP.Registry.create(registry, "shopping")
      {:ok, bucket} = TRYMIXNOTP.Registry.lookup(registry, "shopping")
      Agent.stop(bucket)
      assert TRYMIXNOTP.Registry.lookup(registry, "shopping") == :error
    end
```
The test above will fail on the last assertion as the bucket name remains in the registry even after we stop the bucket process.  
To fix the bug, we need the registry to monitor every bucket it spawns for cleaning the dictionary up every time a bucket exits.

Let's first play with monitors by starting a new console with iex -S mix:
   
    iex> {:ok, pid} = KV.Bucket.start_link
    {:ok, #PID<0.66.0>}
    iex> Process.monitor(pid)
    #Reference<0.0.0.551>
    iex> Agent.stop(pid)
    :ok
    iex> flush()
    {:DOWN, #Reference<0.0.0.551>, :process, #PID<0.66.0>, :normal}

After we stop the agent, we can flush/0 all messages and notice a :DOWN message arrived, with the exact reference returned by monitor, notifying that the bucket process exited with reason :normal.

Let’s reimplement the server callbacks to fix the bug and make the test pass. First, we will modify the GenServer state to two dictionaries: one that contains name -> pid and another that holds ref -> name. Then we need to monitor the buckets on `handle_cast/2` as well as implement a `handle_info/2` callback to handle the monitoring messages.

``` elixir
    ##Server callbacks

    def init(:ok) do
      names = %{}
      refs  = %{}
      {:ok, {names, refs}}
    end

    def handle_call({:lookup, name}, _from, {names, _} = state) do
      {:reply, Map.fetch(names, name), state}
    end

    def handle_cast({:create, name}, {names, refs}) do
      if Map.has_key?(names, name) do
        {:noreply, {names, refs}}
      else
        {:ok, pid} = TRYMIXNOTP.Bucket.start_link
        ref = Process.monitor(pid)
        refs = Map.put(refs, ref, name)
        names = Map.put(names, name, pid)
        {:noreply, {names, refs}}
      end
    end

    def handle_info({:DOWN, ref, :process, _pid, _reason}, {names, refs}) do
      {name, refs} = Map.pop(refs, ref)
      names = Map.delete(names, name)
      {:noreply, {names, refs}}
    end

    def handle_info(_msg, state) do
      {:noreply, state}
    end
```

### `call`, `cast` or `info`?
1. `handle_call/3` must be used for synchronous requests. This should be the default choice as waiting for the server reply is a useful backpressure mechanism.

2. `handle_cast/2` must be used for asynchronous requests, when you don’t care about a reply. A cast does not even guarantee the server has received the message and, for this reason, should be used sparingly. For example, the create/2 function we have defined in this chapter should have used call/2. We have used cast/2 for didactic purposes.

3. `handle_info/2` must be used for all other messages a server may receive that are not sent via `GenServer.call/2` or `GenServer.cast/2`, including regular messages sent with `send/2`. The monitoring `:DOWN` messages are such an example of this.

If there isn't a catch-all clause, some unexpected messages that might arrive to the server could lead our registry to crash.  
We don’t need to worry about such cases for `handle_call/3` and `handle_cast/2` though. Calls and casts are only done via the GenServer API, so an unknown message is quite likely to be due to a developer mistake.   

### Monitor or links?
Links are bi-directional. A monitor is uni-directional: only the monitoring process will receive notifications about the monitored one. 
Thus, use links when you want linked crashes, and monitors when you just want to be informed of crashes, exits, and so on.   

Returning to our `handle_cast/2` implementation, you can see the registry is both linking and monitoring the buckets:

``` elixir
    {:ok, pid} = KV.Bucket.start_link
    ref = Process.monitor(pid)
```
This is a bad idea, as we don’t want the registry to crash when a bucket crashes! We typically avoid creating new processes directly, instead we delegate this responsibility to supervisors.   
As we’ll see in the next chapter, supervisors rely on links and that explains why link-based APIs (`spawn_link`, `start_link`, etc) are so prevalent in Elixir and OTP.
