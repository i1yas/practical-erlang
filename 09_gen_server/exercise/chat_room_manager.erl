-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2]).
-export([call/2, loop/1]).

-record(room, {name, users, messages}).
-record(message, {user, text}).

start() ->
    io:format("start ~p~n", [self()]),
    InitialState = #{},
    spawn(?MODULE, loop, [InitialState]).


create_room(Server, RoomName) ->
    call(Server, {create_room, RoomName}).


remove_room(Server, RoomId) ->
    call(Server, {remove_room, RoomId}).


get_rooms(Server) ->
    call(Server, rooms).


add_user(Server, RoomId, UserName) ->
    call(Server, {add_user, RoomId, UserName}).


remove_user(Server, RoomId, UserName) ->
    call(Server, {remove_user, RoomId, UserName}).


get_users_list(Server, RoomId) ->
    call(Server, {users, RoomId}).


send_message(Server, RoomId, UserName, Message) ->
    call(Server, {message, RoomId, UserName, Message}).


get_messages_history(Server, RoomId) ->
    call(Server, {messages, RoomId}).

call(Server, Msg) ->
    Ref = erlang:monitor(process, Server),
    Server ! {call, Ref, self(), Msg},
    receive
        {reply, Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Server, Reason} ->
            {error, Reason}
    after 5000 ->
            erlang:demonitor(Ref, [flush]),
            noreply
    end.

loop(State) ->
    io:format("~p enters loop ~n", [self()]),
    receive
        {call, Ref, From, Msg} -> {Reply, NewState} = handle_call(Msg, State),
                             From ! {reply, Ref, Reply},
                             ?MODULE:loop(NewState);
        stop -> io:format("~p stops now ~n", [self()]);
        Msg -> io:format("ERROR: ~p receive unknown msg ~p~n", [self(), Msg]),
               ?MODULE:loop(State)
    end.

handle_call({create_room, RoomName}, State) ->
    case (maps:size(State) > 4) of
        true -> {{error, room_limit}, State};
        false ->
            RoomId = make_ref(),
            NewState = State#{RoomId => #room{name = RoomName, users = [], messages = []}},
            {{ok, RoomId}, NewState}
    end;
handle_call({remove_room, RoomId}, State) ->
    case State of
        #{RoomId := _} ->
            NewState = maps:remove(RoomId, State),
            {ok, NewState};
        _ -> {{error, room_not_found}, State}
    end;
handle_call(rooms, State) ->
    RoomList = lists:map(fun({RoomId, #room{name = RoomName}}) -> {RoomId, RoomName} end,
        maps:to_list(State)),
    {RoomList, State};
handle_call({add_user, RoomId, UserName}, State) ->
    case State of
        #{RoomId := Room = #room{users = UserList}} ->
            case lists:member(UserName, UserList) of
                true -> {{error, user_is_in_room}, State};
                false ->
                    NewState = State#{RoomId => Room#room{users = [UserName | UserList]}},
                    {ok, NewState}
            end;
        _ -> {{error, room_not_found}, State}
    end;
handle_call({users, RoomId}, State) ->
    case State of
        #{RoomId := #room{users = UserList}} -> {{ok, UserList}, State};
        _ -> {{error, room_not_found}, State}
    end;
handle_call({remove_user, RoomId, UserName}, State) ->
    case State of
        #{RoomId := #room{users = UserList}} ->
            case lists:member(UserName, UserList) of
                true ->
                    NewState = State#{RoomId => #room{users = lists:delete(UserName, UserList)}},
                    {ok, NewState};
                false -> {{error, user_not_in_room}, State}
            end;
        _ -> {{error, room_not_found}, State}
    end;
handle_call({message, RoomId, UserName, Message}, State) ->
    case State of
        #{RoomId := Room = #room{users = UserList}} ->
            case lists:member(UserName, UserList) of
                true ->
                    NewState = State#{RoomId =>
                        Room#room{messages = [#message{user = UserName, text = Message} | Room#room.messages]}},
                    {ok, NewState};
                false -> {{error, user_not_in_room}, State}
            end;
        _ -> {{error, room_not_found}, State}
    end;
handle_call({messages, RoomId}, State) ->
    case State of
        #{RoomId := #room{messages = Messages}} ->
            MessagesToSend = lists:map(fun(#message{user = User, text = Text}) -> {User, Text} end, Messages),
            {{ok, MessagesToSend}, State};
        _ -> {{error, room_not_found}, State}
    end.