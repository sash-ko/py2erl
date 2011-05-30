-module(sample).
-export([add/2]).

add(0, 0) ->
    0;
add(A, B) ->
    A + B.

add(A, B) ->
    A + B,
    C = A + 10,
    C + B.

other(A, B, C) ->
    A + B - C.

str_smaple() ->
    io:format("~w~n", ["text"]).
