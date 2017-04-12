## [Dependencies and umbrella projects](http://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-apps.html)
In this chapter, we will discuss how to manage dependencies in Mix.   
However, instead of adding more code to the kv application, we are going to build the TCP server as another application that is a client of the kv application.  

Before creating our new application, we must discuss how Mix handles dependencies. In practice, there are two kinds of dependencies we usually work with: internal and external dependencies.   

### External dependencies
External dependencies are the ones not tied to your business domain. For example, if you need a HTTP API for your distributed KV application, you can use the Plug project as an external dependency.  

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
TODO
