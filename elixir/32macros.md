## [Macros](http://elixir-lang.org/getting-started/meta/macros.html)
### Foreword
Macros are harder to write than ordinary Elixir functions and it’s considered to be bad style to use them when they’re not necessary. So write macros responsibly.  
Macros should only be used as a last resort. Remember that **explicit is better than implicit. Clear code is better than concise code**.  

### Our first macro
Macros in Elixir are defined via `defmacro/2`.  
In order to better understand how macros work, let’s create a new module where we are going to implement unless, which does the opposite of `if`, as a macro and as a function:  

``` elixir
    defmodule Unless do
      def fun_unless(clause, do: expression) do
        if(!clause, do: expression)
      end

      defmacro macro_unless(clause, do: expression) do
        quote do
          if(!unquote(clause), do: unquote(expression))
        end
      end
    end
```
The function receives the arguments and passes them to `if`. However, as we learned in the previous chapter, the macro will receive quoted expressions, inject them into the quote, and finally return another quoted expression.

Let’s start iex with the module above:

```
    $ iex macros.exs
```
And play with those definitions:  

``` iex
    iex> require Unless
    iex> Unless.macro_unless true, do: IO.puts "this should never be printed"
    nil
    iex> Unless.fun_unless true, do: IO.puts "this should never be printed"
    "this should never be printed"
    nil
```

Note that in our macro implementation, the sentence was not printed, although it was printed in our function implementation. That’s because the arguments to a function call are evaluated before calling the function. However, macros do not evaluate their arguments. Instead, they receive the arguments as quoted expressions which are then transformed into other quoted expressions. In this case, we have rewritten our unless macro to become an if behind the scenes.  

In other words, when invoked as:  

``` elixir
    Unless.macro_unless true, do: IO.puts "this should never be printed"
```
Our `macro_unless` macro received the following:  

``` elixir
    macro_unless(true, [do: {{:., [], [{:__aliases__, [alias: false], [:IO]}, :puts]}, [], ["this should never be printed"]}])
```
And it then returned a quoted expression as follows:          

``` elixir
    {:if, [],
      [{:!, [], [true]},
       [do: {{:., [],
         [{:__aliases__,
           [], [:IO]},
           :puts]}, [], ["this should never be printed"]}]]}
```

We can actually verify that this is the case by using `Macro.expand_once/2`:

``` elixir
    iex> expr = quote do: Unless.macro_unless(true, do: IO.puts "this should never be printed")
    iex> res  = Macro.expand_once(expr, __ENV__)
    iex> IO.puts Macro.to_string(res)
    if(!true) do
      IO.puts("this should never be printed")
    end
    :ok
```
`Macro.expand_once/2` receives a quoted expression and expands it according to the current environment. In this case, it expanded/invoked the `Unless.macro_unless/2` macro and returned its result. We then proceeded to convert the returned quoted expression to a string and print it (we will talk about `__ENV__` later in this chapter).   

That’s what macros are all about. They are about receiving quoted expressions and transforming them into something else. 

### Macro hygiene
Elixir macros have late resolution. This guarantees that a variable defined inside a quote won’t conflict with a variable defined in the context where that macro is expanded. For example:

``` elixir
    defmodule Hygiene do
      defmacro no_interference do
        quote do: a = 1
      end
    end

    defmodule HygieneTest do
      def go do
        require Hygiene
        a = 13
        Hygiene.no_interference
        a
        end
      end

      HygieneTest.go
      # => 13
```
In the example above, even though the macro injects a = 1, it does not affect the variable a defined by the go function. If a macro wants to explicitly affect the context, it can use var!:  

``` elixir
    defmodule Hygiene do
      defmacro interference do
        quote do: var!(a) = 1
      end
    end

    defmodule HygieneTest do
      def go do
        require Hygiene
        a = 13
        Hygiene.interference
        a
      end
    end

    HygieneTest.go
    # => 1
```
Variable hygiene only works because Elixir annotates variables with their context. For example, a variable x defined on line 3 of a module would be represented as:  

``` elixir
    {:x, [line: 3], nil}
```
However, a quoted variable is represented as:  

``` elixir
    defmodule Sample do
      def quoted do
        quote do: x
      end
    end

    Sample.quoted #=> {:x, [line: 3], Sample}
```
Notice that the third element in the quoted variable is the atom Sample, instead of nil, which marks the variable as coming from the Sample module. Therefore, Elixir considers these two variables as coming from different contexts and handles them accordingly.    

Elixir provides similar mechanisms for imports and aliases too. This guarantees that a macro will behave as specified by its source module rather than conflicting with the target module where the macro is expanded. Hygiene can be bypassed under specific situations by using macros like var!/2 and alias!/2, although one must be careful when using those as they directly change the user environment.  

Sometimes variable names might be dynamically created. In such cases, `Macro.var/2` can be used to define new variables:  

``` elixir
    defmodule Sample do
      defmacro initialize_to_char_count(variables) do
        Enum.map variables, fn(name) ->
          var = Macro.var(name, nil)
          length = name |> Atom.to_string |> String.length
          quote do
            unquote(var) = unquote(length)
          end
        end
      end

      def run do
        initialize_to_char_count [:red, :green, :yellow]
        [red, green, yellow]
      end
    end

    > Sample.run #=> [3, 5, 6]
```
Take note of the second argument to `Macro.var/2`. This is the context being used and will determine hygiene as described in the next section.   

### The environment
`__ENV__` returns an instance of the `Macro.Env` struct which contains useful information about the compilation environment, including the current module, file and line, all variables defined in the current scope, as well as imports, requires and so on:

``` iex
    iex> __ENV__.module
    nil
    iex> __ENV__.file
    "iex"
    iex> __ENV__.requires
    [IEx.Helpers, Kernel, Kernel.Typespec]
    iex> require Integer
    nil
    iex> __ENV__.requires
    [IEx.Helpers, Integer, Kernel, Kernel.Typespec]
```

### Private macros
Elixir also supports private macros via defmacrop. As private functions, these macros are only available inside the module that defines them, and only at compilation time.  

It is important that a macro is defined before its usage. Failing to define a macro before its invocation will raise an error at runtime, since the macro won’t be expanded and will be translated to a function call:   

``` elixir
    iex> defmodule Sample do
    ...>  def four, do: two + two
    ...>  defmacrop two, do: 2
    ...> end
    ** (CompileError) iex:2: function two/0 undefined
```

### Write macros responsibly
1. Macros are hygienic: by default, variables defined inside a macro are not going to affect the user code. Furthermore, function calls and aliases available in the macro context are not going to leak into the user context.  
2. Macros are lexical: it is impossible to inject code or macros globally. In order to use a macro, you need to explicitly require or import the module that defines the macro.  
3. Macros are explicit: it is impossible to run a macro without explicitly invoking it. For example, some languages allow developers to completely rewrite functions behind the scenes, often via parse transforms or via some reflection mechanisms. In Elixir, a macro must be explicitly invoked in the caller during compilation time.
4. Macros’ language is clear: many languages provide syntax shortcuts for quote and unquote. In Elixir, we preferred to have them explicitly spelled out, in order to clearly delimit the boundaries of a macro definition and its quoted expressions.

Even with such guarantees, the developer plays a big role when writing macros responsibly. If you are confident you need to resort to macros, remember that macros are not your API. Keep your macro definitions short, including their quoted contents. For example, instead of writing a macro like this:

``` elixir
    defmodule MyModule do
      defmacro my_macro(a, b, c) do
        quote do
          do_this(unquote(a))
          ...
          do_that(unquote(b))
          ...
          and_that(unquote(c))
        end
      end
    end
    
    # write:

    defmodule MyModule do
      defmacro my_macro(a, b, c) do
        quote do
          # Keep what you need to do here to a minimum
          # and move everything else to a function
          do_this_that_and_that(unquote(a), unquote(b), unquote(c))
        end
          end

      def do_this_that_and_that(a, b, c) do
        do_this(a)
        ...
        do_that(b)
        ...
        and_that(c)
      end
    end
```
