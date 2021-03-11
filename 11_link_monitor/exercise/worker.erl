-module(worker).

parse(MainPid, File) ->
    case file:read_file(File) of
        {ok, FileContent} ->
            MainPid ! {result, #{}};
        {error, Reason} -> exit({File, Reason})
    end.