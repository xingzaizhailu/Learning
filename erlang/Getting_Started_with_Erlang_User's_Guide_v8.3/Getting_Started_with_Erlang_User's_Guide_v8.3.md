# [Getting Started with Erlang User's Guide V8.3](http://erlang.org/doc/getting_started/intro.html)
## 2 Sequential Programming
### 2.1 The Erlang Shell

``` shell
    % erl
    1> 

    2> halt()
```

### 2.2 Modules and Functions

``` erlang
    %% tut.erl
    -module(tut).
    -export([double/1]).

    double(X) ->
        2 * X.
```

``` erl
    1> c(tut).
    2> tut:double(10).

    %% The shell formats the error message nicely, but the error tuple is saved in the shell's history list and can be output by the shell command v/1
    3> tut:double("abc").
    4> v(3).
```
### 2.3 Atoms
``` erlang
    -module(tut2).
    -export([convert/2]).

    convert(M, inch) ->
        M / 2.54;

    convert(N, centimeter) ->
        N * 2.54.
```
### 2.4 Tuples
``` erlang
-module(tuples).
-export([convert_length/1]).

convert_length({centimeter, X}) ->
    {inch, X / 2.54};
convert_length({inch, Y}) ->
    {centimeter, Y * 2.54}.

```

### 2.5 Lists
Erlang does not have a string data type. Strings can be represented by list of Unicode charactoers.  

``` erlang
    30> [97,98,99].
    "abc"
```

### 2.6 Maps
Maps are a set of key to value associations. These associations are encapsulated with "#{" and "}".  

``` erlang
    > #{ "key" => 42 }.
    # {"key" => 42}
```

The syntax for updating an existing key with a new value is with the `:=` operator.  

### 2.7 Standard Modules and Manual Pages
the command `erl -man` can be used at the operating shell or command prompt   

``` erlang
    % erl -man io
```

### 2.8 Writing Output to a Terminal
``` erlang
    1> io:format("hello world~n", []).
    hello world
    ok
    2> io:format("this outputs two Erlang terms: ~w ~w~n", [hello, world]).
    this outputs two Erlang terms: hello world
    ok
```

### 2.9 A Larger Example

### 2.10 Matching, Guards, and Scope of Variables
+ /=  not equal

### 2.11 More About Lists

### 2.12 If and Case
``` erlang
    if
        Condition 1 ->
            Action 1;
        Condition 2 ->
            Action 2;
        Condition 3 ->
            Action 3;
        Condition 4 ->
            Action 4            % no ; or . here
    end
```

``` erlang
    -module(tut9).
    -export([test_if/2]).

    test_if(A, B) ->
        if 
            A == 5 ->
                io:format("A == 5~n", []),
                a_equals_5;
            B == 6 ->
                io:format("B == 6~n", []),
                b_equals_6;
            A == 2, B == 3 ->                      %That is A equals 2 and B equals 3
                io:format("A == 2, B == 3~n", []),
                a_equals_2_b_equals_3;
            A == 1 ; B == 7 ->                     %That is A equals 1 or B equals 7
                io:format("A == 1 ; B == 7~n", []),
                a_equals_1_or_b_equals_7
        end.
```
### 2.13 Built-In Functions(BIFs)
**BIFs** often implement functionality that is impossible or is too inefficient to implement in Erlang. Some **BIFs** can be called using the function name only but they are by default belonging to the `erlang` module.   

``` erlang
    > trunc(5.6).
    5
    > round(5.6).
    6
    > length([a,b,c,d]).
    4
    > float(5).
    5.0
    > is_atom(hello).
    true
    > is_tuple({paris, {c, 30}}).
    true

    > atom_to_list(hello).
    "hello"
    > list_to_atom("goodbye").
    goodbye
    > integer_to_list(22).
    "22"
```

### 2.14 Higher-Order Functions(Funs)
Here is defined a function that doubles the value of a number and assigned this function to a variable.   

``` erlang
    > Xf = fun(X) -> X * 2 end.
    #Fun<erl_eval.5.123085357>
    > Xf(5).
    10
```
Two useful functions when working with lists are foreach and map:

``` erlang
    foreach(Fun, [First|Rest]) ->
        Fun(First),
        foreach(Fun, Rest);
    foreach(Fun, []) ->
        ok.

    map(Fun, [First|Rest]) -> 
        [Fun(First)|map(Fun,Rest)];
    map(Fun, []) -> 
        [].
```

``` erlang
    88> Add_3 = fun(X) -> X + 3 end.
    #Fun<erl_eval.5.123085357>
    89> lists:map(Add_3, [1,2,3]).
    [4,5,6]
```

``` erlang
-module(tut13).

-export([convert_list_to_c/1]).

convert_to_c({Name, {f, Temp}}) ->
    {Name, {c, trunc((Temp - 32) * 5 / 9)}};
convert_to_c({Name, {c, Temp}}) ->
    {Name, {c, Temp}}.

convert_list_to_c(List) ->
    lists:map(fun convert_to_c/1, List).
```

``` erlang
    92> tut13:convert_list_to_c([{moscow, {c, -10}}, {cape_town, {f, 70}},
            {stockholm, {c, -4}}, {paris, {f, 28}}, {london, {f, 36}}]).
    [{moscow,{c,-10}},
     {cape_town,{c,21}},
     {stockholm,{c,-4}},
     {paris,{c,-2}},
     {london,{c,2}}]
```

## 3 Concurrent Programming 
### 3.1 Processes
(Aside: the term "process" is usually used when the threads of execution share no data with each other and the term "thread" when they share data in some way. Threads of execution in Erlang share no data, that is why they are called processes).   
The Erlang BIF `spawn` is used to create a new process: `spawn(Module, Exported_Function, List of Arguments)`.  

``` erlang
-module(tut14).

-export([start/0, say_something/2]).

say_something(What, 0) ->
    done;
say_something(What, Times) ->
    io:format("~p~n", [What]),
    say_something(What, Times - 1).

start() ->
    spawn(tut14, say_something, [hello, 3]),
    spawn(tut14, say_something, [goodbye, 3]).
5> c(tut14).
{ok,tut14}
6> tut14:say_something(hello, 3).
hello
hello
hello
done
```

``` erl
9> tut14:start().
hello
goodbye
<0.63.0>
hello
goodbye
hello
goodbye
```

`spawn` returns a `process identifier`, or `pid`, which uniquely identifies the process. So `<0.63.0>` is the pid of the spawn function call above. The next example shows how to use pids.  

Notice also that `~p` is used instead of `~w` in io:format. To quote the manual: "~p Writes the data with standard syntax in the same way as ~w, but breaks terms whose printed representation is longer than one line into many lines and indents each line sensibly. It also tries to detect lists of printable characters and to output these as strings".

### 3.2 Message Passing
``` erlang
-module(tut15).

-export([start/0, ping/2, pong/0]).

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_PID).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start() ->
    Pong_PID = spawn(tut15, pong, []),
    spawn(tut15, ping, [3, Pong_PID]).
```

``` erl
1> c(tut15).
{ok,tut15}
2> tut15: start().
<0.36.0>
Pong received ping
Ping received pong
Pong received ping
Ping received pong
Pong received ping
Ping received pong
ping finished
Pong finished
```

The receive construct is used to allow processes to wait for messages from other processes. It has the following format:  

``` erlang
    receive
        pattern1 ->
            actions1;
        pattern2 ->
            actions2;
        ....
        patternN
            actionsN        % no ;
    end.
```
Notice how the operator "!" is used to send messages.   

    Pid ! Message

### 3.3 Registered Process Names
``` erlang
    register(some_atom, Pid)
```

``` erlang
-module(tut16).

-export([start/0, ping/1, pong/0]).

ping(0) ->
    pong ! finished,
        io:format("ping finished~n", []);

ping(N) ->
    pong ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start() ->
    register(pong, spawn(tut16, pong, [])),
    spawn(tut16, ping, [3]).
```

### 3.4 Distributed Programming
Use named Erlang system to simulate different computers.

    $ erl -sname my_name

``` erlang
Here is the ping pong example modified to run on two separate nodes:

-module(tut17).

-export([start_ping/1, start_pong/0,  ping/2, pong/0]).

ping(0, Pong_Node) ->
    {pong, Pong_Node} ! finished,
        io:format("ping finished~n", []);

ping(N, Pong_Node) ->
    {pong, Pong_Node} ! {ping, self()},
        receive
            pong ->
                io:format("Ping received pong~n", [])
        end,
        ping(N - 1, Pong_Node).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start_pong() ->
    register(pong, spawn(tut17, pong, [])).

start_ping(Pong_Node) ->
    spawn(tut17, ping, [3, Pong_Node]).
```

# Failed
# TODO

## 4 Robustness
If a node where a user is logged on goes down without doing a logoff, the user remains in the server's `User_List`, but the client disappears. This makes it impossible for the user to log on again as the server thinks the user already is logged on.  
Or what happens if the server goes down in the middle of sending a message, leaving the sending client hanging forever in the `await_result` function?  

### 4.1 Time-outs

``` erlang
-module(tut19).

-export([start_ping/1, start_pong/0,  ping/2, pong/0]).

ping(0, Pong_Node) ->
    io:format("ping finished~n", []);
ping(N, Pong_Node) ->
    {pong, Pong_Node} ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_Node).

pong() ->
    receive
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    after 5000 ->
        io:format("Pong timed out~n", [])
    end.

start_pong() ->
    register(pong, spawn(tut19, pong, [])).

start_ping(Pong_Node) ->
    spawn(tut19, ping, [3, Pong_Node]).
```
# TODO




















 


