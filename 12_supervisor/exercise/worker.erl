-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Id) ->
    gen_server:start_link(?MODULE, Id, []).

ping(Pid) ->
    gen_server:call(Pid, ping).

init(Id) ->
    {ok, Id}.

handle_call(ping, _From, Id) ->
    {reply, {Id, self()}, Id}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.