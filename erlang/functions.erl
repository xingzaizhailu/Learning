-module(functions).
-compile(export_all).

    double(X) ->
        2 * X.

    head([H|_]) -> H.
    second([_, H|_]) -> H.

    tail([H|T]) ->
    t = [H|T] when H != [].

    same(X, X) ->
        true;
    same(_, _) ->
        false.

