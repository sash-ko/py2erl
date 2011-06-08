-module(io_format).
-compile(export_all).

%% {function,4,simple_endl,0,
%%     [{clause,4,[],[],
%%         [{call,5,
%%             {remote,5,{atom,5,io},{atom,5,format}},
%%             [{string,5,"test~n"},{nil,5}]}]}]}
simple_endl() ->
    io:format("test~n", []).

%% {function,7,simple_no_endl,0,
%%     [{clause,7,[],[],
%%         [{call,8,
%%             {remote,8,{atom,8,io},{atom,8,format}},
%%             [{string,8,"test"},{nil,8}]}]}]}
simple_no_endl() ->
    io:format("test", []).

simple_format(Val) ->
    io:format("test ~w, ~w~n", [100, Val]).
