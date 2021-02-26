-module(task_4).

-export([dropwhile/2, takewhile/2]).

-include_lib("eunit/include/eunit.hrl").

reverse(List) -> reverse(List, []).

reverse([], Acc) -> Acc;
reverse([Head | Tail], Acc) -> reverse(Tail, [Head | Acc]).


%% implement lists:dropwhile/2
%% http://www.erlang.org/doc/man/lists.html#dropwhile-2
dropwhile(Pred, List) ->
    dropwhile(Pred, List, []).

dropwhile(_, [], Acc) -> reverse(Acc);
dropwhile(Pred, [Head | Tail], Acc) ->
    Check = Pred(Head),
    if Check -> dropwhile(Pred, Tail, Acc);
    true -> [Head | Tail] end.


dropwhile_test() ->
    F = fun(Val) -> Val =:= 32 end,
    ?assertEqual("hello", dropwhile(F, "   hello")),
    ?assertEqual([], dropwhile(F, [])),
    ?assertEqual([1,2,3], dropwhile(F, [1,2,3])),
    ?assertEqual([3,4], dropwhile(F, [32,3,4])),
    ?assertEqual([3,4], dropwhile(F, [32,32,3,4])),
    ?assertEqual([3,32,4,32], dropwhile(F, [32,32,32,32,32,32,3,32,4,32])),
    ok.


%% implement lists:takewhile/2
%% http://www.erlang.org/doc/man/lists.html#takewhile-2
takewhile(Pred, List) ->
    takewhile(Pred, List, []).

takewhile(_, [], Acc) -> reverse(Acc);
takewhile(Pred, [Head | Tail], Acc) ->
    Check = Pred(Head),
    if Check -> takewhile(Pred, Tail, [Head | Acc]);
    true -> reverse(Acc) end.


takewhile_test() ->
    F = fun(Val) -> Val =:= 32 end,
    ?assertEqual("   ", takewhile(F, "   hello")),
    ?assertEqual([], takewhile(F, [])),
    ?assertEqual([], takewhile(F, [1,2,3])),
    ?assertEqual([32], takewhile(F, [32,3,4])),
    ?assertEqual([32,32], takewhile(F, [32,32,3,4])),
    ?assertEqual([32,32,32,32,32,32], takewhile(F, [32,32,32,32,32,32,3,32,4,32])),
    F2 = fun(Val) -> Val < 5 end,
    ?assertEqual([1,2,3,4], takewhile(F2, [1,2,3,4,5,6,7,8])),
    ok.
