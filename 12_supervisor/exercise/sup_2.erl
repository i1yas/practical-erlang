-module(sup_2).
-behavior(supervisor).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get_worker_spec(Id) ->
    WorkerSpec = #{
        restart => permanent,
        type => worker,
        modules => [worker]
    },
    WorkerSpec#{
        id => Id,
        start => {worker, start_link, [Id]}
    }.

init(_Args) ->
    ChildSpecifications = [
        get_worker_spec(worker_3),
        get_worker_spec(worker_4)
    ],
    SupSpec = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1000
    },
    {ok, {SupSpec, ChildSpecifications}}.

add_worker(WorkerId) ->
    supervisor:start_child(sup_2, get_worker_spec(WorkerId)).

remove_worker(WorkerId) ->
    supervisor:terminate_child(sup_2, WorkerId),
    supervisor:delete_child(sup_2, WorkerId).