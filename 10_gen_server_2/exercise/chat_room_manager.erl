-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1, close_room/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    rooms :: []
    }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_room(RoomName) ->
    gen_server:call(?MODULE, {create_room, RoomName}).

get_rooms() ->
    gen_server:call(?MODULE, get_rooms).

add_user(RoomPid, UserName, UserPid) ->
    gen_server:call(?MODULE, {add_user, RoomPid, UserName, UserPid}).

remove_user(RoomPid, UserPid) ->
    gen_server:call(?MODULE, {remove_user, RoomPid, UserPid}).

get_users(RoomPid) ->
    gen_server:call(?MODULE, {get_users, RoomPid}).

send_message(RoomPid, UserName, Text) ->
    gen_server:call(?MODULE, {send_message, RoomPid, UserName, Text}).

get_history(RoomPid) ->
    gen_server:call(?MODULE, {get_history, RoomPid}).

close_room(RoomPid) ->
    gen_server:call(?MODULE, {close_room, RoomPid}).

init(_Args) ->
    {ok, #state{rooms = []}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call({create_room, RoomName}, _From, State) ->
    {ok, RoomPid} = chat_room:start_link(),
    NewRoom = {RoomName, RoomPid},
    NewState = State#state{rooms = [NewRoom | State#state.rooms]},
    {reply, NewRoom, NewState};
handle_call(get_rooms, _From, State) ->
    {reply, State#state.rooms, State};
handle_call({add_user, RoomPid, UserName, UserPid}, _From, State) ->
    case lists:keysearch(RoomPid, 2, State#state.rooms) of
        {value, _} ->
            chat_room:add_user(RoomPid, UserName, UserPid),
            {reply, ok, State};
        _ -> {reply, {error, room_not_found}, State}
    end;
handle_call({remove_user, RoomPid, UserPid}, _From, State) ->
    case lists:keysearch(RoomPid, 2, State#state.rooms) of
        {value, _} ->
            {reply, chat_room:remove_user(RoomPid, UserPid), State};
        _ ->  {reply, {error, room_not_found}, State}
    end;
handle_call({get_users, RoomPid}, _From, State) ->
    case lists:keysearch(RoomPid, 2, State#state.rooms) of
        {value, _} ->
            {reply, {ok, chat_room:get_users(RoomPid)}, State};
        _ ->  {reply, {error, room_not_found}, State}
    end;
handle_call({send_message, RoomPid, UserName, Text}, _From, State) ->
    case lists:keysearch(RoomPid, 2, State#state.rooms) of
        {value, _} ->
            chat_room:add_message(RoomPid, UserName, Text),
            {reply, ok, State};
        _ ->  {reply, {error, room_not_found}, State}
    end;
handle_call({get_history, RoomPid}, _From, State) ->
    case lists:keysearch(RoomPid, 2, State#state.rooms) of
        {value, _} ->
            {reply, {ok, chat_room:get_history(RoomPid)}, State};
        _ ->  {reply, {error, room_not_found}, State}
    end;
handle_call({close_room, RoomPid}, _From, State) ->
    case lists:keysearch(RoomPid, 2, State#state.rooms) of
        {value, _} ->
            NewState = State#state{rooms = lists:keydelete(RoomPid, 2, State#state.rooms)},
            {reply, ok, NewState};
        _ ->  {reply, {error, room_not_found}, State}
    end.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
