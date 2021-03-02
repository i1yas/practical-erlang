-module(template).

-export([parse/2]).

parse(Str, Data) when is_binary(Str) ->
    Openings = binary:matches(Str, <<"{{">>),
    Closings = binary:matches(Str, <<"}}">>),
    parse(Str, Data, {Openings, Closings}).

parse(Str, _, {[], _}) -> Str;
parse(Str, _, {_, []}) -> Str;
parse(Str, Data,
        {[Open | RestOpenings], [Close | RestClosings]}) ->
    {TStartPos, _} = Open,
    {TEndPos,_} = Close,
    TLen = 2 + TEndPos - TStartPos,
    StartPos = TStartPos + 2,
    Len = TEndPos - TStartPos,
    Name = binary:part(Str, StartPos, Len),
    case Data of
        #{Name := Replace} ->
            NewStr = binary:replace(Str, binary:part(Str, TStartPos, TLen), Replace),
            parse(NewStr, Data, {RestOpenings, RestClosings});
        _ ->
            NewStr = binary:replace(Str, binary:part(Str, TStartPos, TLen), <<"">>),
            parse(NewStr, Data, {RestOpenings, RestClosings})
    end.