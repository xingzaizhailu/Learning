## [Supervisor and Application](http://elixir-lang.org/getting-started/mix-otp/supervisor-and-application.html)
### Our first supervisor
Creating a supervisor is not much different from creating a GenServer. We are going to define a module named TRYMIXNOTP.Supervisor, which will use the Supervisor behaviour, inside the `lib/kv/supervisor.ex` file:

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
We have been working inside an application this entire time. Every time we changed a file and ran mix compile, we could see a Generated try_mix_n_otp app message in the compilation output.

We can find the generated .app file at _build/dev/lib/try_mix_n_otp/ebin/try_mix_n_otp.app. Let’s have a look at its contents:

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
We can also configure the generated .app file by customizing the values returned by the application/0 inside our mix.exs project file. We are going to do our first customization soon.

#### Starting applications
When we define a .app file, which is the application specification, we are able to start and stop the application as a whole. We haven’t worried about this so far for two reasons:

1. Mix automatically starts our current application for us
2. Even if Mix didn’t start our application for us, our application does not yet do anything when it starts

In any case, let’s see how Mix starts the application for us. Let’s start a project console with iex -S mix and try:

``` elixir
    TODO
```
