-module(ff_game_server).

-behaviour(gen_server).

-export([start_link/0, accept/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).

-record(state, {
    listen_socket 
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% gen server callbacks 
init(_Args) ->
    {ok, Port} = application:get_env(port),
    io:format("Start ff_game, listen tcp connections on ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port,
        [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- [player_a, player_b]],
    {ok, #state{listen_socket = ListenSocket}}.

accept(Id, ListenSocket) ->
    io:format("Socket for player #~p waiting~n", [Id]),
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Player #~p connected~n", [Id]),
            AvailableCommands = "FIELD LEFT RIGHT UP DOWN",
            Greeting = unicode:characters_to_binary(io_lib:format("~nHello, ~p~nAwailable commands are ~s~n~n", [Id, AvailableCommands])),
            gen_tcp:send(Socket, <<Greeting/binary, "\r\n">>),
            handle_connection(Id, ListenSocket, Socket);
        E -> io:format("Socket for player #~p can't accept client ~p~n", [Id, E])
    end.

handle_connection(Id, ListenSocket, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Msg0} ->
            Msg = binary:part(Msg0, 0, byte_size(Msg0) - 2),
            Reply = handle(Id, Msg),
            gen_tcp:send(Socket, <<Reply/binary, "\r\n">>),
            handle_connection(Id, ListenSocket, Socket);
        {error, closed} ->
            io:format("Socket for player #~p closed~n", [Id]),
            accept(Id, ListenSocket)
    end.

handle(Player, Msg) ->
    Field = ff_game_player:get_field(),
    case ff_game_player:get_game_status(Field) of
        {game_over, #{Player := IsWinner}} ->
            PlayersStatuses = #{
                true => "winner",
                false => "loser"
            },
            GameoverGoodbye = io_lib:format("You are ~s",
                [maps:get(IsWinner, PlayersStatuses)]),
            unicode:characters_to_binary([ff_game:draw_field(Field), GameoverGoodbye]);
        continue -> parse_command(Player, Msg, Field)
    end.

parse_command(Player, Msg, CurrentField) ->
    Cmd = case Msg of
        <<"LEFT">> -> left;
        <<"RIGHT">> -> right;
        <<"UP">> -> up;
        <<"DOWN">> -> down;
        <<"FIELD">> -> field;
        _ -> unknown
    end,
    case Cmd of
        unknown -> <<"UNKNOWN COMMAND">>;
        field -> ff_game:draw_field(CurrentField);
        ValidCommand ->
            case ff_game_player:move(Player, ValidCommand) of
                {ok, Field} -> ff_game:draw_field(Field);
                {error, invalid_move} -> <<"INVALID MOVE">>;
                E ->
                    io:format("Unknown error: ~p~n", [E]),
                    <<"UNKNOWN ERROR">>
            end
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call(_Request, _From,  State) ->
    {reply, ok, State}.

handle_info(_Request, State) ->
    {noreply, State}.

