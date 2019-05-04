## [Dependencies and umbrella projects](http://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-apps.html)
In this chapter, we will discuss how to manage dependencies in Mix.   
However, instead of adding more code to the `try_mix_n_otp` application, we are going to build the TCP server as another application that is a client of the `try_mix_n_otp` application.  

Before creating our new application, we must discuss how Mix handles dependencies. In practice, there are two kinds of dependencies we usually work with: internal and external dependencies.   

### External dependencies
External dependencies are the ones not tied to your business domain. For example, if you need a HTTP API for your distributed TRYMIXNOTP application, you can use the Plug project as an external dependency.  

Installing external dependencies is simple. Most commonly, we use the Hex Package Manager, by listing the dependency inside the deps function in our mix.exs file:  

``` elixir
    def deps do
      [{:plug, "~> 1.0"}]
      # This dependency refers to the latest version of Plug in the 1.x.x version series that has been pushed to Hex.   
      end
```

Typically, stable releases are pushed to Hex. If you want to depend on an external dependency still in development, Mix is able to manage git dependencies too:

``` elixir
    def deps do
      [{:plug, git: "git://github.com/elixir-lang/plug.git"}]
      end
```
You will notice that when you add a dependency to your project, Mix generates a `mix.lock` file that guarantees `repeatable builds`. The lock file must be checked in to your version control system, to guarantee that everyone who uses the project will use the same dependency versions as you.    

Mix provides many tasks for working with dependencies, which can be seen in `mix help`.    
The most common tasks are mix deps.get and mix deps.update. Once fetched, dependencies are automatically compiled for you.

### Internal dependencies
2 methods to work with them:

    1. git repositories
    discouraged
``` elixir
    def deps do
      [{:try_mix_n_otp, git: "https://github.com/YOUR_ACCOUNT/try_mix_n_otp.git"}]
    end
```
    2. umbrella projects
    
### Umbrella projects
Let’s start a new project named `try_mno_umbrella` using `mix new` and pass the `--umbrella` option when creating it. Do not create this new project inside the existing `try_mix_n_otp` project!  

``` shell
    $ mix new try_mixotp_umbrella --umbrella
    * creating .gitignore
    * creating README.md
    * creating mix.exs
    * creating apps
    * creating config
    * creating config/config.exs
```
From the printed information, we can see far fewer files are generated. The generated `mix.exs` file is different too. Let’s take a look (comments have been removed):  
    
``` elixir
    defmodule TtyMixotpUmbrella.Mixfile do
      use Mix.Project

      def project do
        [apps_path: "apps",
        build_embedded: Mix.env == :prod,
        start_permanent: Mix.env == :prod,
        deps: deps]
      end

      defp deps do
        []
      end
    end
```
What makes this project different from the previous one is the `apps_path: "apps"` entry in the project definition. This means this project will act as an umbrella. Such projects do not have source files nor tests, although they can have their own dependencies. Each child application must be defined inside the apps directory.     

Let's move inside the apps directory and start building `try_mixotp_server`. This time, we are going to pass the `--sup` flag, which will tell Mix to generate a supervision tree automatically for us, instead of building one manually as we did in previous chapters:

     $ cd try_mixotp_umbrella/apps
     $ mix new try_mixotp_server --module TRYMIXOTPServer --sup

The generated files are similar to the ones we first generated for `try_mix_n_otp`, with a few differences. Let’s open up `mix.exs`:  

``` elixir 
    defmodule TRYMIXOTPServer.Mixfile do
      use Mix.Project

      def project do
        [app: :try_mixotp_server,
         version: "0.1.0",

         # something extra because inside umbrella
         build_path: "../../_build",
         config_path: "../../config/config.exs",
         deps_path: "../../deps",
         lockfile: "../../mix.lock",

         elixir: "~> 1.4",
         build_embedded: Mix.env == :prod,
         start_permanent: Mix.env == :prod,
         deps: deps]
      end

      def application do
        [extra_applications: [:logger],
        mod: {TRYMIXOTPServer.Application, []}]
      end

      defp deps do
        []
      end
    end
```
Those extra options mean all dependencies will be checked out to `try_mixotp_umbrella/deps`, and they will share the same build, config and lock files. This ensures dependencies will be fetched and compiled once for the whole umbrella structure, instead of once per umbrella application.    

The second change is in the application function inside mix.exs:  

``` elixir
    def application do
      [extra_applications: [:logger],
      mod: {TRYMIXOTPServer.Application, []}]
    end
```
Because we passed the `--sup` flag, Mix automatically added `mod: {TRYMIXOTPServer.Application, []}`, specifying that `TRYMIXOTPServer.Application` is our application callback module. `TRYMIXOTPServer.Application` will start our application supervision tree.   

In fact, let’s open up `lib/kv_server/application.ex`:

``` elixir
    defmodule TRYMIXOTPServer.Application do
      # See http://elixir-lang.org/docs/stable/elixir/Application.html
      # for more information on OTP Applications
      @moduledoc false

      use Application

      def start(_type, _args) do
        import Supervisor.Spec, warn: false

        # Define workers and child supervisors to be supervised
        children = [
          # Starts a worker by calling: TRYMIXOTPServer.Worker.start_link(arg1, arg2, arg3)
          # worker(TRYMIXOTPServer.Worker, [arg1, arg2, arg3]),
        ]

        # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
        # for other strategies and supported options
        opts = [strategy: :one_for_one, name: TRYMIXOTPServer.Supervisor]
        Supervisor.start_link(children, opts)
      end
    end
```
Notice that it defines the application callback function, `start/2`, and instead of defining a supervisor named `TRYMIXOTPServer.Supervisor` that uses the `Supervisor` module, it conveniently defined the supervisor inline!    

#### In umbrella dependencies
Mix supports an easy mechanism to make one umbrella child depend on another. Open up apps `  `try_mixotp_server/mix.exs` and change the `deps/0` function to the following:

``` elixir
    defp deps do
      [{:try_mixotp, in_umbrella: true}]
    end
```

Copy try_mix_n_otp application to the `apps` directory. Then modify `apps/try_mix_n_otp/mix.exs` to contain the umbrella entries we have seen in `app/try_mixopt_server/mix.exs`.
