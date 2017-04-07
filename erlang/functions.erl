-module(functions).
-compile(export_all).

    head([H|_]) -> H.
    second([_, H|_]) -> H.

    tail([H|T]) ->
        t = [H|t] when H != [];

    same(X, X) ->
        true;
    same(_, _) ->
        false;

