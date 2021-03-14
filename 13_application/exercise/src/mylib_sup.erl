-module(mylib_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupSpec = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1000
    },
    {ok, {SupSpec, [#{
            id => worker,
            start => {mylib_worker, start_link, []},
            type => worker
        }]}}.
