-module(template).

-export([parse/2]).

match_template(Str, start) ->
    binary:match(Str, <<"{{">>);
match_template(Str, finish) ->
    binary:match(Str, <<"}}">>).

parse(Str, Data) when is_binary(Str) ->
    parse(Str, Data,
        {match_template(Str, start), match_template(Str, finish)}).

parse(Str, _, {nomatch, _}) -> Str;
parse(Str, _, {_, nomatch}) -> Str;
parse(Str, Data, {{TStartPos, _}, {TEndPos, _}}) ->
    TLen = 2 + TEndPos - TStartPos,
    StartPos = TStartPos + 2,
    Len = TLen - 4,
    Name = binary:part(Str, StartPos, Len),
    case Data of
        #{Name := ReplaceRaw} ->
            Replace = if 
                is_binary(ReplaceRaw) -> ReplaceRaw;
                is_integer(ReplaceRaw) -> integer_to_binary(ReplaceRaw);
                is_list(ReplaceRaw) -> unicode:characters_to_binary(ReplaceRaw)
            end,
            NewStr = binary:replace(Str, binary:part(Str, TStartPos, TLen), Replace),
            parse(NewStr, Data,
                {match_template(NewStr, start), match_template(NewStr, finish)});
        _ ->
            NewStr = binary:replace(Str, binary:part(Str, TStartPos, TLen), <<"">>),
            parse(NewStr, Data,
                {match_template(NewStr, start), match_template(NewStr, finish)})
    end.