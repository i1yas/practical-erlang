-module(mylib_worker).
-behavior(gen_server).

-export([start_link/0, get_version/0, get_modules/0, get_min_val/0, get_connection_timeout/0, all_apps/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_version() ->
    gen_server:call(mylib_worker, get_version).

get_modules() ->
    gen_server:call(mylib_worker, get_modules).

get_min_val() ->
    gen_server:call(mylib_worker, get_min_val).

get_connection_timeout() ->
    gen_server:call(mylib_worker, get_connection_timeout).

all_apps() ->
    gen_server:call(mylib_worker, all_apps).

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call(get_version, _From, State) ->
    {ok, Result} = application:get_key(mylib, vsn),
    {reply, Result, State};
handle_call(get_modules, _From, State) ->
    {ok, Result} = application:get_key(modules),
    {reply, Result, State};
handle_call(get_min_val, _From, State) ->
    {ok, Result} = application:get_env(min_val),
    {reply, Result, State};
handle_call(get_connection_timeout, _From, State) ->
    {ok, Result} = application:get_env(connection_timeout),
    {reply, Result, State};
handle_call(all_apps, _From, State) ->
    Apps = application:which_applications(),
    FormatedApps = lists:foldl(fun
        ({Name, _, Version}, Acc) ->
            {ok, Description} = application:get_key(description),
            Acc#{Name => #{ description => Description, version => Version }}
        end, #{}, Apps),
    {reply, FormatedApps, State}.

handle_info(_Request, State) ->
    {noreply, State}.

init(_Args) ->
    {ok, ok}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.