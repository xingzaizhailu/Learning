## [Supervisor and Application](http://elixir-lang.org/getting-started/mix-otp/supervisor-and-application.html)
### Our first supervisor
Creating a supervisor is not much different from creating a GenServer. We are going to define a module named TRYMIXNOTP.Supervisor, which will use the Supervisor behaviour, inside the `lib/TRYMIXNOTP/supervisor.ex` file:

``` elixir
    defmodule TRYMIXNOTP.Supervisor do
      use Supervisor

      def start_link do
        Supervisor.start_link(__MODULE__, :ok)
      end

      def init(:ok) do
        children = [
          # start a process using the following call:
          # TRYMIXNOTP.Registry.start_link(TRYMIXNOTP.Registry)
          #                                         |
          #                                         --> name of the process
          worker(TRYMIXNOTP.Registry, [TRYMIXNOTP.Registry])
        ]

        supervise(children, strategy: :one_for_one)
      end
    end
```
Because supervise process might crash, in which case its Pid will change when the supervisor restarts it. By using a name, we can guarantee the newly started process will register itself under the same name, without a need to explicity fetch the latest pid.   

Finally, we call `supervise/2`, passing the list of children and the strategy of `:one_for_one`.

The supervision strategy dictates what happens when one of the children crashes. `:one_for_one` means that if a child dies, it will be the only one restarted.   

Since `TRYMIXNOTP.Registry.start_link/1` is now expecting an argument, we need to change our implementation to receive such argument. Open up `lib/TRYMIXNOTP/registry.ex` and replace the `start_link/0` definition by:

``` elixir
    @doc """
    Starts the registry with the given `name`.
    """
    def start_link(name) do
      GenServer.start_link(__MODULE__, :ok, name: name)
    end
```
We also need to update our tests to give a name when starting the registry. Replace the setup function in `test/TRYMIXNOTP/registry_test.exs` by:

``` elixir
    setup context do
      {:ok, registry} = TRYMIXNOTP.Registry.start_link(context.test)
      {:ok, registry: registry}
    end
```
The context includes some default keys, like `:case`, `:test`, `:file` and `:line`.   
If we start a console inside our project using iex -S mix, we can manually start the supervisor:

```elixir
    iex> TRYMIXNOTP.Supervisor.start_link
    {:ok, #PID<0.66.0>}
    iex> TRYMIXNOTP.Registry.create(TRYMIXNOTP.Registry, "shopping")
    :ok
    iex> TRYMIXNOTP.Registry.lookup(TRYMIXNOTP.Registry, "shopping")
    {:ok, #PID<0.70.0>}
```
When we started the supervisor, the registry worker was automatically started, allowing us to create buckets without the need to manually start it.

In practice we rarely start the application supervisor manually. Instead it is started as part of the application callback.


### Understanding applications
We have been working inside an application this entire time. Every time we changed a file and ran mix compile, we could see a Generated `try_mix_n_otp app` message in the compilation output.

We can find the generated .app file at `_build/dev/lib/try_mix_n_otp/ebin/try_mix_n_otp.app`. Let’s have a look at its contents:

``` elixir
    {application,try_mix_n_otp,
                [{applications,[kernel,stdlib,elixir,logger]},
                 {description,"try_mix_n_otp"},
                 {modules,['Elixir.try_mix_n_otp','Elixir.try_mix_n_otp.Bucket',
                           'Elixir.try_mix_n_otp.Registry','Elixir.try_mix_n_otp.Supervisor']}]}.
                 {registered,[]},
                 {vsn,"0.1.0"},
                 {extra_application, [logger]}]}.
```
We can also configure the generated `.app` file by customizing the values returned by the `application/0` inside our `mix.exs` project file. We are going to do our first customization soon.  
#### Starting applications
When we define a `.app` file, which is the application specification, we are able to start and stop the application as a whole. We haven’t worried about this so far for two reasons:

1. Mix automatically starts our current application for us
2. Even if Mix didn’t start our application for us, our application does not yet do anything when it starts

In any case, let’s see how Mix starts the application for us. Let’s start a project console with `iex -S mix` and try:

``` elixir
    iex> Application.start(:try_mix_n_otp)
    {:error, {:already_started, :try_mix_n_otp}}
```

We can pass an option to Mix to ask it to not start our application. Let’s give it a try by running `iex -S mix run --no-start`:

``` iex
    iex> Application.start(:try_mix_n_otp)
    :ok
```

We can stop our `:try_mix_n_otp` application as well as the `:logger` application, which is started by default with Elixir:

    iex> Application.stop(:try_mix_n_otp)
    :ok
    iex> Application.stop(:logger)
    :ok
And let’s try to start our application again:

``` iex
    iex> Application.start(:try_mix_n_otp)
    {:error, {:not_started, :logger}}
```
Now we get an error because an application that `:try_mix_n_otp` depends on (`:logger` in this case) isn’t started. We need to either start each application manually in the correct order or call `Application.ensure_all_started` as follows:

    iex> Application.ensure_all_started(:try_mix_n_otp)
    {:ok, [:logger, :try_mix_n_otp]}

#### The application callback
We can configure the application callback in two steps. First, open up the mix.exs file and change def application to the following:

``` elixir
    def application do
      [extra_applications: [:logger],
      mod: {TRYMIXNOTP, []}]
    end
```
The `:mod` option specifies the “application callback module”, followed by the arguments to be passed on application start. The application callback module can be any module that implements the Application behaviour.   

Now that we have specified `TRYMIXNOTP` as the module callback, we need to change the `TRYMIXNOTP` module, defined in `lib/try_mix_n_otp.ex`:   

``` elixir
    defmodule TRYMIXNOTP do
      use Application

      def start(_type, _args) do
        TRYMIXNOTP.Supervisor.start_link
      end
    end
```

Let’s start our project console once again with `iex -S mix`. We will see a process named `TRYMIXNOTP`.Registry is already running:

``` iex
    iex> TRYMIXNOTP.Registry.create(TRYMIXNOTP.Registry, "shopping")
    :ok
    iex> TRYMIXNOTP.Registry.lookup(TRYMIXNOTP.Registry, "shopping")
    {:ok, #PID<0.88.0>}
```

For an experiment, try reimplementing TRYMIXNOTP.Registry.create/2 to use GenServer.call/3 instead, and momentarily disable the application callback. Run the code above on the console again, and you will see the creation step fail straightaway.   

Don’t forget to bring the code back to normal before resuming this tutorial!

#### Projects or applications?
Based on the contents of our `mix.exs` file, we would say we have a Mix project that defines the `:try_mix_n_otp` application. As we will see in later chapters, there are projects that don’t define any application.   
When we say “project” you should think about Mix. Mix is the tool that manages your project. It knows how to compile your project, test your project and more. It also knows how to compile and start the application relevant to your project.   

### Simple one for one supervisors
We have now successfully defined our supervisor which is automatically started (and stopped) as part of our application lifecycle.   

Remember however that our `TRYMIXNOTP.Registry` is both linking and monitoring `bucket` processes in the `handle_cast/2` callback:

``` elixir
    {:ok, pid} = TRYMIXNOTP.Bucket.start_link
    ref = Process.monitor(pid)
```

Links are bi-directional, which implies that a crash in a bucket will crash the registry. Although we now have the supervisor, which guarantees the registry will be back up and running, crashing the registry still means we lose all data associating bucket names to their respective processes.   

In other words, we want the registry to keep on running even if a bucket crashes. Let’s write a new registry test:   

``` elixir
    test "removes bucket on crash", %{registry: registry} do
      TRYMIXNOTP.Registry.create(registry, "shopping")
      {:ok, bucket} = TRYMIXNOTP.Registry.lookup(registry, "shopping")

      # Stop the bucket with non-normal reason
      ref = Process.monitor(bucket)
      Process.exit(bucket, :shutdown)

      # Wait until the bucket is dead
      assert_receive {:DOWN, ^ref, _, _, _}

      assert TRYMIXNOTP.Registry.lookup(registry, "shopping") == :error
    end
```

Opposite to `Agent.stop/1`, `Process.exit/2` is an asynchronous operation, therefore we cannot simply query `TRYMIXNOTP.Registry.lookup/2` right after sending the exit signal because there will be no guarantee the bucket will be dead by then.   
Since the bucket is linked to the registry, which is then linked to the test process, killing the bucket causes the registry to crash which then causes the test process to crash too:   

```
    1) test removes bucket on crash (TRYMIXNOTP.RegistryTest)
       test/try_mix_n_otp/registry_test.exs:52
       ** (EXIT from #PID<0.94.0>) shutdown
```
One possible solution to this issue would be to provide a `TRYMIXNOTP.Bucket.start/0`, that invokes `Agent.start/1`, and use it from the registry, removing the link between `registry` and `buckets`. However, this would be a bad idea because buckets would not be linked to any process after this change. This means that, if someone stops the `:try_mix_n_otp` application, all buckets would remain alive as they are unreachable. Not only that, if a process is unreacheable, they are harder to introspect.      

We are going to solve this issue by defining a new supervisor that will spawn and supervise all buckets. There is one supervisor strategy, called `:simple_one_for_one`, that is the perfect fit for such situations: it allows us to specify a `worker template` and supervise many children based on this template. With this strategy, no workers are started during the supervisor initialization, and a new worker is started each time `start_child/2` is called.

Let’s define our `TRYMIXNOTP.Bucket.Supervisor` in `lib/try_mix_n_otp/bucket/supervisor.ex` as follows:

``` exlir
    defmodule TRYMIXNOTP.Bucket.Supervisor do
      use Supervisor

      # A simple module attribute that stores the supervisor name
      @name TRYMIXNOTP.Bucket.Supervisor

      def start_link do
          Supervisor.start_link(__MODULE__, :ok, name: @name)
      end

      def start_bucket do
        Supervisor.start_child(@name, [])
      end

      def init(:ok) do
        children = [
          worker(TRYMIXNOTP.Bucket, [], restart: :temporary)
        ]

        supervise(children, strategy: :simple_one_for_one)
      end
    end
```

There are three changes in this supervisor compared to the first one.   

First of all, we have decided to give the supervisor a local name of `TRYMIXNOTP.Bucket`.Supervisor. We have also defined a `start_bucket/0` function that will start a bucket as a child of our supervisor named `TRYMIXNOTP.Bucket.Supervisor`. `start_bucket/0` is the function we are going to invoke instead of calling `TRYMIXNOTP.Bucket.start_link` directly in the registry.

Finally, in the `init/1` callback, we are marking the worker as `:temporary`. This means that if the bucket dies, it won’t be restarted. That’s because we only want to use the supervisor as a mechanism to group the buckets.

Run iex -S mix so we can give our new supervisor a try:   

``` iex
    iex> {:ok, _} = TRYMIXNOTP.Bucket.Supervisor.start_link
    {:ok, #PID<0.70.0>}
    iex> {:ok, bucket} = TRYMIXNOTP.Bucket.Supervisor.start_bucket
    {:ok, #PID<0.72.0>}
    iex> TRYMIXNOTP.Bucket.put(bucket, "eggs", 3)
    :ok
    iex> TRYMIXNOTP.Bucket.get(bucket, "eggs")
    3
```

Let’s change the registry to work with the buckets supervisor by rewriting how buckets are started:   

``` elixir
    def handle_cast({:create, name}, {names, refs}) do
      if Map.has_key?(names, name) do
        {:noreply, {names, refs}}
      else
        {:ok, pid} = TRYMIXNOTP.Bucket.Supervisor.start_bucket
        ref = Process.monitor(pid)
        refs = Map.put(refs, ref, name)
        names = Map.put(names, name, pid)
        {:noreply, {names, refs}}
      end
    end
```
Once we perform those changes, our test suite should fail as there is no bucket supervisor. Instead of directly starting the bucket supervisor on every test, let’s automatically start it as part of our main supervision tree.   

### Supervisor trees
In order to use the buckets supervisor in our application, we need to add it as a child of `TRYMIXNOTP.Supervisor`. Notice we are beginning to have supervisors that supervise other supervisors, forming so-called “supervision trees”.   

Open up lib/try_mix_n_otp/supervisor.ex and change init/1 to match the following:   

``` elixir
    def init(:ok) do
      children = [
        worker(TRYMIXNOTP.Registry, [TRYMIXNOTP.Registry]),
        supervisor(TRYMIXNOTP.Bucket.Supervisor, [])
      ]

      supervise(children, strategy: :one_for_one)
    end
```
Since we have added more children to the supervisor, it is also important to evaluate if the :`one_for_one` supervision strategy is still correct.    
If `TRYMIXNOTP.Registry` dies, all information linking `TRYMIXNOTP.Bucket` names to `TRYMIXNOTP.Bucket` processes is lost, and therefore `TRYMIXNOTP.Bucket.Supervisor` must die too- otherwise, the `TRYMIXNOTP.Bucket` processes it manages would be orphaned.      

In light of this observation, we should consider moving to another supervision strategy. The two other candidates are `:one_for_all` and `:rest_for_one`. A supervisor using the `:one_for_all` strategy will kill and restart all of its children processes whenever any one of them dies. At first glance, this would appear to suit our use case, but it also seems a little heavy-handed, because TRYMIXNOTP.Registry is perfectly capable of cleaning itself up if TRYMIXNOTP.Bucket.Supervisor dies. In this case, the `:rest_for_one` strategy comes in handy: when a child process crashes, the supervisor will only kill and restart child processes which were started after the crashed child. Let’s rewrite our supervision tree to use this strategy instead:

``` elixir
    def init(:ok) do
      children = [
        worker(TRYMIXNOTP.Registry, [TRYMIXNOTP.Registry]),
        supervisor(TRYMIXNOTP.Bucket.Supervisor, [])
      ]

      supervise(children, strategy: :rest_for_one)
    end
```
Now, if the registry worker crashes, both the registry and the “rest” of `TRYMIXNOTP.Supervisor`’s children (i.e. `TRYMIXNOTP.Bucket.Supervisor`) will be restarted. However, if `TRYMIXNOTP.Bucket.Supervisor` crashes, `TRYMIXNOTP.Registry` will not be restarted, because it was started prior to `TRYMIXNOTP.Bucket.Supervisor`.  

### Observer
Start your application with iex -S mix and key this in:   

``` iex
    iex> :observer.start
```
In the Applications tab, you will see all applications currently running in your system along side their supervision tree. You can select the `try_mix_n_otp` application to explore it further:   
Not only that, as you create new buckets on the terminal, you should see new processes spawned in the supervision tree shown in Observer:

``` iex
    iex> TRYMIXNOTP.Registry.create TRYMIXNOTP.Registry, "shopping"
    :ok
```
### Shared state in tests
So far we have been starting one registry per test to ensure they are isolated:

``` elixir
    setup context do
      {:ok, registry} = TRYMIXNOTP.Registry.start_link(context.test)
      {:ok, registry: registry}
    end
```

Since we have now changed our registry to use `TRYMIXNOTP.Bucket.Supervisor`, which is registered globally, our tests are now relying on this shared, global supervisor even though each test has its own registry. The question is: should we?

It depends. It is ok to rely on shared global state as long as we depend only on a non-shared partition of this state. For example, every time we register a process under a given name, we are registering a process against a shared name registry. However, as long as we guarantee the names are specific to each test, by using a construct like context.test, we won’t have concurrency or data dependency issues between tests.

Similar reasoning should be applied to our bucket supervisor. Although multiple registries may start buckets on the shared bucket supervisor, those buckets and registries are isolated from each other. We would only run into concurrency issues if we used a function like `Supervisor.count_children`(`TRYMIXNOTP.Bucket.Supervisor`) which would count all buckets from all registries, potentially giving different results when tests run concurrently.

Since we have relied only on a non-shared partition of the bucket supervisor so far, we don’t need to worry about concurrency issues in our test suite. In case it ever becomes a problem, we can start a supervisor per test and pass it as an argument to the registry `start_link` function.

Now that our application is properly supervised and tested, let’s see how we can speed things up.











