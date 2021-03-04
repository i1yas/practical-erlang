-module(template).

-export([parse/2]).


parse(Str, Data) when is_binary(Str) ->
    Splited = binary:split(Str, <<"{{">>, [global]),
    Replaced = lists:map(fun(Chunk) ->
            case binary:split(Chunk, <<"}}">>) of
                [NoSep] -> NoSep;
                [Name | Rest] ->
                    case Data of
                        #{Name := Replace} when is_number(Replace) -> [integer_to_binary(Replace) | Rest];
                        #{Name := Replace} -> [Replace | Rest];
                        _ -> [Rest]
                    end
            end
        end, Splited),
    unicode:characters_to_binary(Replaced).
