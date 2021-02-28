-module(solution).

-export([
    get_users/0, get_sessions/0, group_by_sex/1,
    group_by_fun/2, get_sessions_by_node/1, get_sessions_by_type/1]).

get_users() ->
    [
    {user, "Bob", 21, male},
    {user, "Bill", 23, male},
    {user, "Helen", 17, female},
    {user, "Kate", 25, female},
    {user, "John", 20, male}].

get_sessions() ->
    [
    {session, type_a, node_1, 101},
    {session, type_b, node_1, 202},
    {session, type_a, node_2, 303},
    {session, type_b, node_2, 404}].
 

group_by_sex(Users) -> 
    group_by_sex(Users, #{male => [], female => []}).

group_by_sex([], Groups) -> Groups;
group_by_sex([User | RestUsers], Groups) ->
    case User of
        {_, _, _, male} ->
            group_by_sex(RestUsers, Groups#{male => [User | maps:get(male, Groups)]});
        {_, _, _, female} ->
            group_by_sex(RestUsers, Groups#{female => [User | maps:get(female, Groups)]})
    end.

group_by_fun(CriteriaFun, Users) ->
    group_by_fun(CriteriaFun, Users, #{}).

group_by_fun(_, [], Groups) -> Groups;
group_by_fun(CriteriaFun, [User | RestUsers], Groups) ->
    GroupKey = CriteriaFun(User),
    case Groups of
        #{GroupKey := GroupList} ->
            group_by_fun(CriteriaFun, RestUsers, Groups#{GroupKey => [User | GroupList]});
        _ ->
            group_by_fun(CriteriaFun, RestUsers, Groups#{GroupKey => [User]})
    end.

get_sessions_by_node(Sessions) ->
    group_by_fun(fun({_, _, Node, _}) -> Node end, Sessions).

get_sessions_by_type(Sessions) ->
    group_by_fun(fun({_, Type, _, _}) -> Type end, Sessions).