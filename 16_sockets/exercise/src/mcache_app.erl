-module(mcache_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    application:start(mcache),
    ok.

start(_StartType, _StartArgs) ->
    mcache:start_link().

stop(_App) ->
    ok.