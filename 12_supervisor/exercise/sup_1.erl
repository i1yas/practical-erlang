-module(sup_1).
-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    WorkerSpec = #{
        restart => permanent,
        type => worker,
        modules => [worker]
    },
    ChildSpecifications = [
        WorkerSpec#{
            id => worker_1,
            start => {worker, start_link, [worker_1]}
        },
        WorkerSpec#{
            id => worker_2,
            start => {worker, start_link, [worker_2]}
        }
    ],
    SupSpec = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1000
    },
    {ok, {SupSpec, ChildSpecifications}}.