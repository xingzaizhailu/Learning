-module(process31).
-export([start/0, say_something/2]).

say_something(_What, 0) ->
    done;
say_something(What, Times) ->
    io:format("~p~n", [What]),
    say_something(What, Times - 1).

%% process31:start().
start() ->
    spawn(process31, say_something, [hello, 3]),
    spawn(process31, say_something, [goodbye, 3]).
