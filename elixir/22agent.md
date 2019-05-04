## [Agent](http://elixir-lang.org/getting-started/mix-otp/agent.html)
We'll create a TRYMIXNOTP.Bucket module which will be responsible for storing our key-value entries in a way that allows them to be read and modified by other processes.
### The trouble with state
If we want to provide state, where we create buckets putting and reading values from multiple places, we have two main options in Elixir:
    1. Processes
        + Agent - Simple wrappers around state
        + GenServer - "Generic servers"(processes) that encapsulate state, provide sync and async calls, support code reloading, and more.
        + GenEvent - "Generic event" managers that allow publishing events to multiple handlers.
        + Task - Asynchronous units of computation that allow spawing a process and potentially retrieving its result at a later time.
    These are all implemented on top of processes using the basic features provided by the VM, like `send`, `receive`, `spawn`, `link`.
    2. ETS(Erlang Term Storage)

### Agent
Agents are simple wrappers around state. If all you want from a process is to keep state, agents are a great fit. Start an `iex` session inside the project with:

    $ iex -S mix
And play a bit with agents:
    
    iex> {:ok, agent} = Agent.start_link fn -> [] end
    {:ok, #PID<0.57.0>}
    iex> Agent.update(agent, fn list -> ["eggs" | list] end)
    :ok
    iex> Agent.get(agent, fn list -> list end)
    ["eggs"]
    iex> Agent.stop(agent)
    :ok

Let's implement our TRYMIXNOTP.Bucket using agents. Before that, let's write some tests.   
Create a file at `test/try_mix_n_otp/bucket_test.exs` with the following:
``` elixir
    defmodule TRYMIXNOTP.BucketTest do 
      use ExUnit.Case, async: true
      # the test sync: true option makes the test case run in parallel with other :async test cases

      test "stores values by key" do

        {:ok, bucket} = TRYMIXNOTP.Bucket.start_link
        assert TRYMIXNOTP.Bucket.get(bucket, "mild") == nil

        TRYMIXNOTP.Bucket.put(bucket, "milk", 30
        assert TRYMIXNOTP.Bucket.get(bucket, "milk") == 3
      end
    end
```

Create `lib/try_mix_n_otp/bucket.ex`:

``` elixir
    defmodule TRYMIXNOTP.Bucket do
      @doc """
      Starts a new bucket.
      """
      def start_link do
        Agent.start_link(fn -> %{} end)
      end

      @doc """
      Gets a value from the `bucket` by `key`.
      """
      def get(bucket, key) do
        Agent.get(bucket, &Map.get(&1, key))
      end

      @doc """
      Puts the `value` for the given `key` in the `bucket`.
      """
      def put(bucket, key, value) do
        Agent.update(bucket, &Map.put(&1, key, value))
      end
    end
```
Try it by running: `$ mix test`

### Test setup with ExUnit callbacks
All TRYMIXNOTP.Bucket tests will require a bucket to be started during setup and stopped after the test. Luckily, ExUnit supports callbacks that allow us to skip such repetitive tasks.   

Let’s rewrite the test case to use callbacks:

``` elixir
    defmodule TRYMIXNOTP.BucketTest do
      use ExUnit.Case, async: true

      setup do
        {:ok, bucket} = TRYMIXNOTP.Bucket.start_link
        {:ok, bucket: bucket}
      end

      test "stores values by key", %{bucket: bucket} do
        assert TRYMIXNOTP.Bucket.get(bucket, "milk") == nil

        TRYMIXNOTP.Bucket.put(bucket, "milk", 3)
        assert TRYMIXNOTP.Bucket.get(bucket, "milk") == 3
      end
    end
```

### Other agent actions
get a value and pudate the agent state in one func call vai `Agent.get_and_update/2`.   
Let's implement a `TRYMIXNOTP.Bucket.delete/2` func:
    
``` elixir
    @doc """
    Deletes `key` from `bucket`.

    Returns the current value of `key`, if `key` exists.
    """
    def delete(bucket, key) do
      Agent.get_and_update(bucket, &Map.pop(&1, key))
    end
```

### Client/Server in agents
Expand the `delete/2` func we have just implemented:
    
``` elixir
    def delete(bucket, key) do
      Agent.get_and_update(bucket, fn dict ->
        Map.pop(dict, key)
      end)
    end
```
Everything that is inside the function we passed to the agent happens in the agent process. In this case, since the agent process is the one receiving and responding to our message, we say the agent process is the server. Everything outside the func is happening in the client.  
This distinction is **important**. If there are expensive actions to be done, you must consider if it will be better to perform these actions on the client or on the server. E.g.:

``` elixir    
    def delete(bucket, key) do
      Process.sleep(1000)       # puts client to sleep
      Agent.get_and_update(bucket, fn dict ->
        Process.sleep(1000)     # puts server to sleep
        Map.pop(dict, key)
      end)
    end
```
