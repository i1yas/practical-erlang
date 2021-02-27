%% Реализовать функцию, которая принимает список, и возвращает максимальный элемент этого списка
-module(list_max).

-export([list_max/1]).

list_max(List) ->
    lists:foldl(fun(Item, Max) ->
        case Item > Max of
            true -> Item;
            false -> Max
        end
    end, 0, List).