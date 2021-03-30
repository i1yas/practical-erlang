-module(ff_game_player).

-behaviour(gen_server).

-export([start_link/0, get_field/0, move/2, get_game_status/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).

-record(state, {
    game_over,
    field
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_field() ->
    gen_server:call(?MODULE, get_field).

move(Player, Cmd) ->
    gen_server:call(?MODULE, {move, Player, Cmd}).

-spec get_game_status(ff_game:field()) -> continue | {game_over, #{player_a => boolean(), player_b => boolean()}}.
get_game_status(Field) ->
    gen_server:call(?MODULE, {get_game_status, Field}).

% gen server api 
init(_Args) ->
    {ok, {W, H}} = application:get_env(field_size),
    Field = ff_game:initial_field(W, H),
    {ok, #state{field = Field}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call(get_field, _From,  State) ->
    {reply, State#state.field, State};
handle_call({get_game_status, Field}, _From,  State) ->
    PlayerStatuses = ff_game:get_player_statuses(Field),
    case PlayerStatuses of
        #{player_a := true, player_b := true} ->
            {reply, continue, State};
        _ ->
            {reply, {game_over, PlayerStatuses}, State#state{game_over = true}}
    end;
handle_call({move, Player, Cmd}, _From,  State) ->
    Result = ff_game:move(Player, Cmd, State#state.field),
    case Result of
        {ok, Field} ->
            NewState = State#state{field = Field},
            {reply, Result, NewState};
        E -> {reply, E, State}
    end;
handle_call(_Request, _From,  State) ->
    {reply, ok, State}.

handle_info(_Request, State) ->
    {noreply, State}.

