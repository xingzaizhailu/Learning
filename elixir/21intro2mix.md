## [Introduction to Mix](http://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
The application works as a distributed key-value store. We are going to organize key-value pairs into buckets and distribute those buckets across multiple nodes.   
In order to build our key-value application, we are going to use three main tools:
+ OTP(Open Telecom Platform) is a set of libraries that ships with Erlang so does some aspects integrate with Elixir, including supervision trees, event managers and more;
+ Mix is a build tool that ships with Elixir that provides tasks for creating, compiling, testing your application, managing its dependencies and much more;
+ ExUnit is a test-unit based framework that ships with Elixir.  
### Our first project
The main module should be the all-uppercase.  

    $ mix new try_mix_n_otp --module TRYMIXNOTP

`mix.exs`'s main responsibility is to configure our project.  
    + defines two public functions: `project`, which returns project configuration like the project name and version, and `application`, which is used to generate an application file.
    + and a private func named `deps`
`lib/try_mix_n_otp.ex` with a simple module definition.

### Project compilation
    $ cd try_mix_n_otp
    $ mix compile
The lib/try_mix_n_otp.ex file was compiled, an application manifest named try_mix_n_otp.app was generated.   
All compilation artifacts are placed inside the `_build` directory using the options defined in the `mix.exs` file.   
Once the project is compiled, you can start an iex session inside the project by running:

    $ iex -S mix

### Running tests
Mix projects usually follow the convention of having a `<filename>_test.exs` file in the test directory for each file in the lib directory.   
For this reason, we can already find a `test/try_mix_n_otp.exs` corresponding to our `lib/try_mix_n_otp.ex` file.   
Mix also generated a file named `test/test_helper.exs` which is responsible for setting up the test framework:

    ExUnit.start()
We can run tests with `mix test`
Run a specific test with `mix test test/*_test.exs`

### Environments
Allow a developer to customize compilation and the other options for specific scenarios.
By defaults:
    + :dev - the one in which Mix tasks(like `compile`) run by default
    + :test - used by mix test
    + :prod - the one you will use to run your project in production
The environment applies only to the current project. As we will see later on, any dependency you add to your project will by default run in the `:prod` environment.  
Customization per environment can be done by accessing the Mix.env function in your `mix.exs` file.   
The environment can be change d via the `MIX_ENV` environment variable.

    $ MIX_ENV = prod mix compile
    > set "MIX_ENV=prod" && mix compile   # on Windows

### Exploring
    $ mix help
    $ mix help TASK     # get futher info about a particular task
