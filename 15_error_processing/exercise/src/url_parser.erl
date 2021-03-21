-module(url_parser).

-export([parse/1]).


parse_protocol(URL) ->
    case re:run(URL, "^(\\w+)://.*", [{capture, all, binary}]) of
        {match, [_, Protocol]} -> Protocol;
        _ -> throw(invalid_protocol)
    end.
parse_domain(URL) ->
    case re:run(URL, "^\\w+://(.+\\..+?)/.*", [{capture, all, binary}]) of
        {match, [_, Domain]} -> Domain;
        _ -> throw(invalid_domain)
    end.
parse_path(URL) ->
    case re:run(URL, "^\\w+://.+?/(.+)", [{capture, all, binary}]) of
        {match, [_, RawPathAndQuery]} ->
            {P, Q} = case binary:split(RawPathAndQuery, <<"?">>) of
                [RawPath, Query] -> {RawPath, Query};
                [RawPath] -> {RawPath, <<>>};
                _ -> {<<>>, <<>>}
            end,
            Path = lists:filter(fun(Item) -> Item =/= <<>> end,
                binary:split(P, <<"/">>, [global])),
            {Path, Q};
        _ -> {[], <<>>}
    end.
parse_date(Path) ->
    case Path of
        [Year, Month, Day | _] ->
            try
                [Y, M, D] = [list_to_integer(binary_to_list(X)) || X <- [Year, Month, Day]],
                true = (M >= 1) and (M =< 12) and (D >= 1) and (D =< 31),
                {Y, M, D}
            catch
                _:_ -> undefined
            end;
        _ -> undefined
    end.


-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(URL) ->
    try
        Protocol = parse_protocol(URL),
        Domain = parse_domain(URL),
        {Path, Query} = parse_path(URL),
        Date = parse_date(Path),
        Result = #{
            protocol => Protocol,
            domain => Domain,
            path => Path,
            date => Date,
            query => Query
        },
        {ok, Result}
    catch
        throw:invalid_protocol -> {error, invalid_protocol};
        throw:invalid_domain -> {error, invalid_domain}
    end.
