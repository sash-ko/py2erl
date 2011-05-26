-module(erl2abstract).
-export([to_abstract/1]).

to_abstract(File) ->
    {ok, Form} = epp:parse_file(File, [], []),
    Form.
