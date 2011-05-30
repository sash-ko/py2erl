-module(compile_forms).
-export([compile/1]).

compile(Forms) ->
    case compile:forms(Forms) of
        {ok, ModuleName, Binary} -> 
                    save_binary(atom_to_list(ModuleName), Binary);
        {ok, ModuleName, Binary, _Warnings} ->
                    save_binary(atom_to_list(ModuleName), Binary)
    end.

save_binary(ModuleName, Binary) ->
    case file:open(string:concat(ModuleName, ".beam"), [binary, write]) of
        {ok, F} ->
            case file:write(F, Binary) of
                ok ->
                    file:close(F);
                {error, _} = E ->
                    file:close(F),
                    E
            end;
        {error, _} = E -> E
    end.
