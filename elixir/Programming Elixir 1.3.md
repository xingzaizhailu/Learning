# Programming [Elixir](http://elixir-lang.org/) 1.3
## Chap 2: Pattern Matching
## Chap 3: Immutability
Elixir data types are immutable  
## Chap 4: Elixir Basics
### Built-in Types
#### Value Types:
##### Integers: 
Can be written as decimal(`123`), hexadecimal(`0xcafe`), octal(`0o765`), and binary(`0b10110`)   

- no fixed limited on the size!  

##### Floating-Point Numbers:
- Floats are IEEE 754 double precision, about 16 digits of accuracy and a maximum exponent of around 10<sup>308</sup>  

##### Atoms:
- An atom word is a sequence of letters, digits, underscores, and at signs(`@`)
- An atom’s name is its value. Two atoms with the same name will always compare as being equal, even if they were reated by different applications on two computers separated by an ocean
- using a leading colon(`:`)
- followed by an atom word or an Elixir operator.
- end with ! or ? as optional

	> e.g. =>  :fred  :is_binary?  :var@2  :<>  :===  :"func/3"  :"long john silver"

##### Ranges：
* Ranges are represented as **start..end**, where start and end are _integers_.  

##### Regular Expressions:
    ~r{regexp}
    # or
    ~{regexp}opts. { and } can be flexible like ~r/.../

| Option | Meaning																	                 |
| :----: |:----------------------------------------------------------------------------------------- |
| f	     | Force the pattern to start to match on the first line of a multiline string.              |
| g		 | Support named groups. 													                 |
| i		 | Make matches case insensitive.											                 |
| m      | If the string to be matched contains multiple lines, `^` and `$` match the start and end of these lines. `\A` and `\z` continue to match the beginning or end of the string.                                  |
| s      | Allow `.` to match any newline characters.                                                |
| U      |Normally modifiers like `*` and `+` are greedy, matching as much as possible. The `U` modifier makes them ungreedy, matching as little as possible.                                                            |
| u      |Enable unicode-specific patterns like `\p`.                                                |
| x      | Enable extended mode—ignore whitespace and comments (`#` to end of line).                 |

#### System Types: reflect resources in the Erlang VM
##### PIDs and Ports
##### References
The function `make_ref` creates a globally unique reference; no other reference will be equal to it.  

####　Collection Types：
#####　Tuples
A tuple is an ordered collection of values.  
Tuple **cannot** be modified once created.  
A typical Elixir tuple has two to four elements.  
Write a tuple between braces, separating the elements with commas.    

    { 1, 2 }  
    { :ok, 42, "next" }  
    { :error, :enoent }

（Tuple is more like an array）

##### Lists
A linked data structure.  
Lists are easy to traverse linearly, but they are expensive to access in random order (to get to the nth element, you have to scan through n–1 previous elements).  
It is always cheap to get the head of a list and to extract the tail of a list.  

Elixir has some operators that work specifically on lists:  `++`, `--`, `in`

    iex> [ 1, 2, 3 ] ++ [ 4, 5, 6 ] # concatenation
    [1, 2, 3, 4, 5, 6]
    iex> [1, 2, 3, 4] -- [2, 4] # difference
    [1, 3]
    iex> 1 in [1,2,3,4] # membership
    true
    iex> "wombat" in [1, 2, 3, 4]
    false

###### Keyword Lists:
If we write

    [ name: "Dave", city: "Dallas", likes: "Programming" ]
Elixir converts it into a list of two-value tuples:
  
    [ {:name, "Dave"}, {:city, "Dallas"}, {:likes, "Programming"} ]

Elixir allows us to leave off the square brackets if a keyword list is the last argument in a function call. Thus,

    DB.save record, [ {:use_transaction, true}, {:logging, "HIGH"} ]
can be written more cleanly as

    DB.save record, use_transaction: true, logging: "HIGH"

##### Maps
A map is a collection of key/value pairs. A map literal looks like this:  

    %{ key => value, key => value }
Key can be a _string_, _tuple_ or _atom_, and they can also been combined in one map.  
_Maps_ allow only one entry for a particular key, whereas _keyword lists_ allow the key to **be repeated**.  
Maps are **efficient** (particularly as they grow), and they can be used in Elixir’s pattern matching.

**In general, use _keyword lists_ for things such as command-line parameters and for passing around options, and use maps when you want an associative array.**

###### Accessing a Map:
    iex> states = %{ "AL" => "Alabama", "WI" => "Wisconsin" }  
    %{"AL" => "Alabama", "WI" => "Wisconsin"}   
    iex> states["AL"]
    "Alabama"
    iex> states["TX"]
    nil

If the keys are atoms, you can also use a dot notation:

    iex> colors[:red]
    16711680
    iex> colors.green
    65280
You’ll get a KeyError if there’s no matching key when you use the dot notation.

##### Binaries:
enclosed between `<<` and `>>`

    iex> bin = << 1, 2 >>
    <<1, 2>>
    iex> byte_size bin
    2

Here’s a single byte that contains three fields of widths 2, 4, and 2 bits. (The example uses some built-in libraries to show the result’s binary value.)

    iex> bin = <<3 :: size(2), 5 :: size(4), 1 :: size(2)>>
    <<213>>
    iex> :io.format("~-8.2b~n", :binary.bin_to_list(bin))
    11010101
    :ok
    iex> byte_size bin
    1

#### Dates and Times
    iex> d1 = Date.new(2016, 12, 25)
    {:ok, ~D[2016-12-25]}
    iex> {:ok, d1} = Date.new(2016, 12, 25)
    {:ok, ~D[2016-12-25]}
    iex> d2 = ~D[2016-12-25]
    ~D[2016-12-25]
    iex> d1 == d2
    true
    iex> d1
    ~D[2016-12-25]
    iex> inspect d1, structs: false
    "%{__struct__: Date, calendar: Calendar.ISO, day: 25, month: 12, year: 2016}"

The Time type contains an hour, minute, second, and fractions of a second.  
The fraction is stored as a tuple containing microseconds and the number of significant digits.

    iex(35)> t1 = Time.new(12, 34, 56)
    {:ok, ~T[12:34:56]}
    iex(36)> t2 = ~T[12:34:56.78]
    ~T[12:34:56.78]
    iex(37)> t1 == t2
    false
    iex(38)> inspect t2, structs: false
    "{:ok, %{__struct__: Time, hour: 12, microsecond: {780000, 2},
    minute: 34, second: 56}}"

#### Boolean:
_true_, _false_, _nil_(treated as _false_)  
Any value other than false or nil is treated as true, called **truthy**  

    a or b    # true if a is true, otherwise b
    a and b
    not a

#### Operators: 
##### Comparison operators:
`===` `!==` `==` `!=` `>`  `>=` `<` `<=`
> number < atom < reference < function < port < pid <tuple < map < list < bitstring
##### Relaxed Boolean operators：
    a || b     # a if a is truthy, otherwise b
    a && b
    !a
##### Arithmetic operators:
`+`  `-`  `*`  `/`  `div`  `rem`  
div() & rem() return integer
##### Join operators: 
`<>` `++` `--`

##### in operator:
    a in enum

#### Variable Scope
Elixir is lexically scoped. The basic unit of scoping is the function body.  

##### `with` Expression:( to Read Again!)
1. define a local scope for variable
2. control over pattern matching failures  

        variable = with tempVariable = ...,     # There must has clause right after `with`
                             tempV2  <- ...
                   do
                            ...
                   end     // or do: ...
or
        variable = with( 
                        tempVariable = ...,
                        tempV2  <- ...
                   do
                            ...
                   end)     // or do: ...
> Tip: use `<-` instead of `=` to avoid _error exception_ in pattern match

## Chap 5: Anonymous Functions
    fn
         parameter-list -> body
         parameter-list -> body ...
    end

or $ elixir fileName.exs

### Functions can return functions:  

    iex> fun1 = fn -> (fn -> "hello" end) end
    iex> fun2 = fun1.()
    Hello
    :ok

### Closure:

    iex> add_n = fn n -> (fn other -> n + other end) end
    iex> add_two = add_n.(2)
    iex> add_five = add_n.(5)
    iex> add_two.(3) # 5
    iex> add_five.(7) # 12

### 将函数作为参数来传递：  

    times_2 = fn n -> n * 2 end
    apply = fn（func, value) -> func.(value) end
    apply.(times_2, 6) #12

### &运算符：将函数传递给其他函数极好的方式  

    square = &(&1 * &1)
    speak = &(IO.puts(&1))  # IO.puts/1
    speak.("hello") # hello \n :ok
    divrem = &{ div(&1, &2), rem(&1, &2) }
    divrem.(13,5) # {2, 3}

    len = &length/1 # &:erlang.length/1
    len.([1,3,5,7]) # 4
    l = &Enum.count/1 # &Enum.count/1
    l.([1, 2, 3, 4]) # 4







## Something others:
returns are all FLOAT  

  round()
  trunc(3.58) # -> 3

#check for a valua type  

  is_boolean(true) #-> true
  is_boolean(1)    #-> false

h function/operators/...  

IO.puts/1  

Strings are internally binaries, sequences of bytes  

  byte_size("hello") -> 
  String.length/1    ->
  String.upcase("hello")
String.  tab键查看其下方法  

  is_function()
  is_funciotn(func_name, num) # check if f is a f of num arguments

i/1  # to retrieve info about data.(term/data/description/...)  
