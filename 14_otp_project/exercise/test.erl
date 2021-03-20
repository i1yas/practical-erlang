-module(test).
-export([int2bin/1]).

int2bin(Int) when (Int < 16) -> <<"0", (integer_to_binary(Int, 16))/binary>>;
int2bin(Int) -> integer_to_binary(Int, 16).
