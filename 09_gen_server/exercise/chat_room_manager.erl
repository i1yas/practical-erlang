-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2]).
-export([call/2, loop/1]).

start() ->
    io:format("start ~p~n", [self()]),
    InitialState = #{rooms => [], users => [], room_to_users => #{}},
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
    call(Server, {remove_user, {RoomId, UserName}}).


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
    {ok, State};
    % Rooms = maps:get(rooms, State),
    % RoomId = erlang:md5(RoomName),
    % case lists:keysearch(RoomId, 1, Rooms) of
    %     {value, _} ->
    %         NewState = State#{rooms => [{RoomId, RoomName} | Rooms]},
    %         {RoomId, NewState};
    %     false -> {{error, room_limit}, State}
    % end;
handle_call({remove_room, RoomId}, State) ->
    {ok, State};
    % Rooms = maps:get(rooms, State),
    % case list:keysearch(RoomId, 1, Rooms) of
    %     {value, _} ->
    %         NewState = lists:keydelete(RoomId, 1, Rooms),
    %         {ok, NewState};
    %     false -> {{error, not_found}, State}
    % end;
handle_call(rooms, State) ->
    maps:get(rooms, State);
handle_call({add_user, RoomId, UserName}, State) ->
    {ok, State}.
    % Rooms = maps:get(rooms, State),
    % case lists:keysearch(RoomId, 1, Rooms) of
    %     {value, _} ->
    %         RoomToUsers = maps:get(room_to_users, State),
    %         RoomUsers = case RoomToUsers of
    %             #{RoomId := Users} -> Users;
    %             _ -> []
    %         end,
    %         case lists:member(UserName, RoomUsers) of
    %             true -> {{error, user_is_in_room}, State};
    %             false ->
    %                 NewState = State#{room_to_users =>
    %                     RoomToUsers#{RoomId => [UserName | RoomUsers]}},
    %                 {ok, NewState}
    %         end;
    %     false -> {{error, room_not_found}, State}
    % end.