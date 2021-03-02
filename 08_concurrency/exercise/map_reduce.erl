-module(map_reduce).

-export([count_words/1, start/1]).

count_words(Binary) ->
    Lines = binary:split(Binary, [<<" ">>, <<"\n">>], [global]),
    lists:foldl(fun(Word, Acc) ->
            case Acc of
                #{Word := Count} -> Acc#{Word => Count + 1};
                _ -> Acc#{Word => 1}
            end
        end, #{}, Lines).

map(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            WordsCount = count_words(Binary),
            reduce ! {data, WordsCount};
        {error, _Reason} -> reduce ! error
    end.

reduce(0, Acc) -> Acc;
reduce(IterCount, Acc) ->
    receive
        {data, Data} ->
            Merged = maps:fold(fun(Word, Count, MergeAcc) ->
                    case MergeAcc of
                        #{Word := SumCount} -> MergeAcc#{Word => SumCount + Count};
                        _ -> MergeAcc#{Word => Count}
                    end
                end, Acc, Data),
            reduce(IterCount - 1, Merged);
        error -> reduce(IterCount - 1, Acc)
    after 1000 -> io:format("No messages in 1s")
    end.

map_reduce(Files) ->
    [spawn(fun() -> map(F) end) || F <- Files],
    reduce(length(Files), #{}).

start(Files) ->
    register(reduce, self()),
    map_reduce(Files).
