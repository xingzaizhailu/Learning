-module(reccursion).
-export([factorial/1,length1/1]).

factorial(N) when N == 0 -> 1;      %% shortened by: factorial(0) -> 1;
factorial(N) when N > 0  -> N * factorial(N-1). 
    
length1([]) -> 0;
length1([_|T]) -> 1 + length1(T).
