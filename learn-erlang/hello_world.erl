-module(hello_world).
-export([add/2,hello/0,greet_and_add_two/1,double/1,mult/2]).
    
%% c(hello_wolrd)
%% hello_world:add(2, 3)
add(A, B) ->
    A + B.

double(X) ->
    2 * X.

%% shows greetings.
hello() ->
    io:format("hello world!~n").

mult(X, Y) ->
    X * Y.

greet_and_add_two(X) ->
    hello(),
    add(X, 2),
    double(X).
