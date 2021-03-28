%%%-------------------------------------------------------------------
%% @doc ff_game public API
%% @end
%%%-------------------------------------------------------------------

-module(ff_game_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ff_game_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
