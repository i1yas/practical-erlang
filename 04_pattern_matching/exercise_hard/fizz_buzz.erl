%% Реализовать Fizz Buzz
%% https://habr.com/ru/post/298134/
-module(fizz_buzz).

-export([fizzbuzz/1]).

fizzbuzz(N) ->
    lists:map(fun
        (X) when (X rem 3 =:= 0) andalso (X rem 5 =:= 0) -> fizzbuzz;
        (X) when X rem 3 =:= 0 -> fizz;
        (X) when X rem 5 =:= 0 -> buzz;
        (X) -> X end,
    lists:seq(1, N)).
