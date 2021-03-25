-module(mcache).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).
-export([accept/2, handle_connection/3, query/1]).

-define(PORT, 1234).

-record(state, {values}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

accept(Id, ListenSocket) ->
    io:format("Socket #~p wait for client~n", [Id]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Socket #~p, session started~n", [Id]),
    handle_connection(Id, ListenSocket, Socket).

handle_connection(Id, ListenSocket, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Msg} ->
            io:format("Socket #~p got message: ~p~n", [Id, Msg]),
            gen_tcp:send(Socket, unicode:characters_to_binary([query(Msg), "\r\n"])),
            handle_connection(Id, ListenSocket, Socket);
        {error, closed} ->
            io:format("Client on socket #~p closed connection~n", [Id]),
            accept(Id, ListenSocket);
        Any ->
            io:format("Socket #~p got unknown message: ~p~n", [Id, Any]),
            accept(Id, ListenSocket)
    end.

query(RawBin) ->
    Bin = binary:replace(RawBin, <<"\r\n">>, <<>>),
    Query = binary:split(Bin, <<" ">>, [global]),
    case Query of
        [<<"GET">>, Key] ->
            gen_server:call(?MODULE, {get, Key});
        [<<"GETS">>, Key | Keys] ->
            gen_server:call(?MODULE, {gets, [Key | Keys]});
        [<<"SET">>, Key | Value ] ->
            gen_server:call(?MODULE, {set, Key, lists:join(" ", Value)});
        [<<"DELETE">>, Key] ->
            gen_server:call(?MODULE, {delete, Key});
        [<<"ADD">>, Key | Value] -> 
            gen_server:call(?MODULE, {add, Key, lists:join(" ", Value)});
        [<<"REPLACE">>, Key | Value] -> 
            gen_server:call(?MODULE, {replace, Key, lists:join(" ", Value)});
        [<<"APPEND">>, Key | Value] -> 
            gen_server:call(?MODULE, {append, Key, lists:join(" ", Value)});
        [<<"PREPEND">>, Key | Value] -> 
            gen_server:call(?MODULE, {prepend, Key, lists:join(" ", Value)});
        _ -> <<"UNKNOWN REQUEST">> 
    end.

% gen server

init(_Args) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, 5)],
    {ok, #state{values = #{}}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call({get, Key}, _From, State) ->
    case State#state.values of
        #{Key := Value} ->
            {reply, ["VALUE ", Key, " ", Value, "\r\n", "END"], State};
        _ -> {reply, ["NOT FOUND"], State}
    end;
handle_call({gets, Keys}, _From, State) ->
    Values = lists:map(fun
        (Key) ->
            case State#state.values of
                #{Key := Value} -> ["VALUE ", Key, " ", Value, "\r\n"];
                _ -> ["VALUE ", Key, " ", "NOT FOUND\r\n"]
            end
        end,
        Keys),
    {reply, [Values, "END"], State};
handle_call({set, Key, Value}, _From, State) ->
    Values = State#state.values,
    NewState = State#state{values = Values#{ Key => Value }},
    {reply, ["STORED"], NewState};
handle_call({delete, Key}, _From, State) ->
    Values = State#state.values,
    case Values of
        #{Key := _} ->
            NewState = State#state{values = maps:remove(Key, Values)},
            {reply, ["DELETED"], NewState};
        _ -> {reply, ["NOT FOUND", State]}
    end;
handle_call({add, Key, Value}, _From, State) ->
    Values = State#state.values,
    case Values of
        #{Key := _} -> {reply, ["EXISTS"], State};
        _ ->
            NewState = State#state{values = Values#{ Key => Value }},
            {reply, ["STORED"], NewState}
    end;
handle_call({replace, Key, Value}, _From, State) ->
    Values = State#state.values,
    case Values of
        #{Key := _} ->
            NewState = State#state{values = Values#{ Key => Value }},
            {reply, ["STORED"], NewState};
        _ ->
            {reply, ["NOT FOUND"], State}
    end;
handle_call({append, Key, Value}, _From, State) ->
    Values = State#state.values,
    case Values of
        #{Key := CurrentValue} ->
            NewState = State#state{values = Values#{ Key => [CurrentValue, Value] }},
            {reply, ["STORED"], NewState};
        _ ->
            {reply, ["NOT FOUND"], State}
    end;
handle_call({prepend, Key, Value}, _From, State) ->
    Values = State#state.values,
    case Values of
        #{Key := CurrentValue} ->
            NewState = State#state{values = Values#{ Key => [Value, CurrentValue] }},
            {reply, ["STORED"], NewState};
        _ ->
            {reply, ["NOT FOUND"], State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Request, State) ->
    {noreply, State}.