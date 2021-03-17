-module(my_crypt_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    application:start(my_crypt),
    ok.

start(_StartType, _StartArgs) ->
    my_crypt_sup:start_link().

stop(_App) ->
    ok.