-module(main_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    ChildSpecifications = [
        #{id => sup_1, start => {sup_1, start_link, []}, type => supervisor},
        #{id => sup_2, start => {sup_2, start_link, []}, type => supervisor}
    ],
    SupSpec = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1000
    },
    {ok, {SupSpec, ChildSpecifications}}.