-module(main).

-export([parse/1]).

accumulate({Results, Errors}) ->
    receive
        {result, Result} ->
            NewResults = maps:fold(fun(Key, Value, Acc) ->
                    case Acc of
                        #{Key := AccValue} -> Acc#{Key => AccValue + Value};
                        _ -> Acc#{Key => Value}
                    end
                end, Results, Result),
            {NewResults, Errors};
        {error, File, Msg} -> {Results, Errors#{File => Msg}}
    end.

parse(Files) ->
    [spawn_link(worker, parse, [self(), File]) || File <- Files],
    accumulate({#{}, #{}}).
