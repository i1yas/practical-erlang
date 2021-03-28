-module(ff_game_player).

-behaviour(gen_server).

-export([start_link/0, move/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2]).

-record(state, {
    field
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

move(Player, Cmd) ->
    gen_server:call(?MODULE, {move, Player, Cmd}).

% gen server api 
init(_Args) ->
    {ok, {W, H}} = application:get_env(field_size),
    Field = ff_game:initial_field(W, H),
    {ok, #state{field = Field}}.

handle_cast(_Request, State) ->
    {noreply, State}.

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

