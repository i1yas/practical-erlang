-module(main).

-export([parse/1, parse_file/2]).

parse_file(MainPid, File) ->
    {ok, FileContent} = file:read_file(File),
    Result = lists:foldl(fun
        (<<>>, Acc) -> Acc;
        (Line, Acc) ->
            [ _, Name, Count, _ ] = binary:split(Line, <<",">>, [global]),
            Acc#{Name => binary_to_integer(Count)}
        end,
        #{},
        binary:split(FileContent, <<"\n">>, [global])),
    MainPid ! {result, Result}.

accumulate(Workers, {Results, Errors}) ->
    receive
        {result, Result} ->
            NewResults = maps:fold(fun(Key, Value, Acc) ->
                    case Acc of
                        #{Key := AccValue} -> Acc#{Key => AccValue + Value};
                        _ -> Acc#{Key => Value}
                    end
                end, Results, Result),
            accumulate(Workers, {NewResults, Errors});
        {'DOWN', _, process, _, normal} -> accumulate(Workers, {Results, Errors});
        {'DOWN', Ref, process, Worker, Reason} ->
            File = maps:get({Ref, Worker}, Workers),
            accumulate(Workers, {Results, Errors#{File => Reason}})
    after 500 ->
        {Results, Errors}
    end.

parse(Files) ->
    Workers = lists:foldl(fun(File, Acc) ->
            Worker = spawn(?MODULE, parse_file, [self(), File]),
            Ref = erlang:monitor(process, Worker),
            Acc#{{Ref, Worker} => File}
        end, #{}, Files),
    accumulate(Workers, {#{}, #{}}).
