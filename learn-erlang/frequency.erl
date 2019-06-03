-module(frequency).
-export([]).
-export([init/0]).

%% these are the start functions used to create and 
%% initialize the server

start() ->
    register(frequency, spqwn(frequency, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

%% Hard coded
get_frequencies() -> [10,11,12,13,14,15].

%% The client Functions
stop()           -> call(stop).
allocate()       -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

%% We hide all message passing and the message
%% protocol in a functional interface.
call(Message) ->
    frequency ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.
