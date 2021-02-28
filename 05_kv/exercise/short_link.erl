-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1]).

%%% module API

init() ->
    %% init randomizer
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exsp, {A,B,C}),
    State = #{},
    State.


create_short(LongLink, State) ->
    case State of
        #{LongLink := ShortLink} -> {ShortLink, State};
        _ ->
            NewShortLink = "http://hexlet.io/" ++ rand_str(8),
            {NewShortLink,
                State#{LongLink => NewShortLink, NewShortLink => LongLink}}
    end.

get_long(ShortLink, State) ->
    case State of
        #{ShortLink := LongLink} -> {ok, LongLink};
        _ -> {error, not_found}
    end.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [rand:uniform(110 - 48) + 47 || _ <- lists:seq(1, Length)]).
