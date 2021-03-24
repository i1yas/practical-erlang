-module(mcache).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).
-export([accept/2, handle_connection/3, query/1]).

-define(PORT, 1234).

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

query(Bin) ->
    Query = binary:split(Bin, <<" ">>),
    case Query of
        [<<"GET">>, Key] -> ["OK", Key];
        _ -> <<"UNKNOWN REQUEST">> 
    end.

% gen server

init(_Args) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, 5)],
    {ok, nostate}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {replay, ok, State}.

handle_info(_Request, State) ->
    {noreply, State}.