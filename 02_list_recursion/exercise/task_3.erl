-module(task_3).

-export([filter/2, member/2]).

-include_lib("eunit/include/eunit.hrl").

reverse(List) -> reverse(List, []).

reverse([], Acc) -> Acc;
reverse([Head | Tail], Acc) -> reverse(Tail, [Head | Acc]).

%% implement lists:member/2
%% http://www.erlang.org/doc/man/lists.html#member-2
member(_, []) -> false;
member(Elem, [Head | Tail]) ->
    if Elem =:= Head -> true;
       true -> member(Elem, Tail)
    end.

member_test() ->
    ?assertEqual(true, (member(55, [1, 2, 55, 77]))),
    ?assertEqual(false, (member(55, []))),
    ?assertEqual(false, (member(55, [1, 2, 77]))),
    ?assertEqual(true, (member("ab", ["dd", "bd", "ab"]))),
    ok.

%% implement lists:filter/2
%% http://www.erlang.org/doc/man/lists.html#filter-2
filter(Pred, List) -> filter(Pred, List, []).

filter(_, [], Acc) -> reverse(Acc);
filter(Pred, [Head | Tail], Acc) ->
    Check = Pred(Head),
    if Check -> filter(Pred, Tail, [Head | Acc]);
       true -> filter(Pred, Tail, Acc)
    end.

% list comprehention
% filter(Pred, List) -> [X || X <- List, Pred(X)].

filter_test() ->
    F = fun (Val) -> Val rem 5 =:= 0 end,
    ?assertEqual([], (filter(F, []))),
    ?assertEqual([], (filter(F, [1, 2, 3, 4]))),
    ?assertEqual([5, 10],
		 (filter(F, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))),
    ok.
