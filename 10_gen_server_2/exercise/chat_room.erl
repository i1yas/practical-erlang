-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(user, {
    name :: binary(),
    pid :: pid()
    }).
-record(message, {
    username :: binary(),
    text :: binary()
    }).
-record(state, {
    users :: [],
    messages :: []
    }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

add_user(RoomPid, UserName, UserPid) ->
    gen_server:cast(RoomPid, {add_user, UserName, UserPid}),
    ok.

remove_user(RoomPid, UserPid) ->
    gen_server:call(RoomPid, {remove_user, UserPid}).

get_users(RoomPid) ->
    gen_server:call(RoomPid, get_users).

add_message(RoomPid, UserName, Text) ->
    gen_server:cast(RoomPid, {add_message, UserName, Text}).

get_history(RoomPid) ->
    gen_server:call(RoomPid, get_history).

init(_Args) ->
    {ok, #state{messages = [], users = []}}.

handle_cast({add_user, UserName, UserPid}, State) ->
    NewUsers = [#user{name = UserName, pid = UserPid} | State#state.users],
    NewState = State#state{users = NewUsers},
    {noreply, NewState};
handle_cast({add_message, UserName, Text}, State) ->
    NewMessages = [#message{username = UserName, text = Text} | State#state.messages],
    NewState = State#state{messages = NewMessages},
    [chat_user:add_message(UserPid, UserName, Text) || #user{pid = UserPid} <- State#state.users],
    {noreply, NewState}.

handle_call({remove_user, UserPid}, _From, State) ->
    case lists:keysearch(UserPid, #user.pid, State#state.users) of
        {value, _} ->
            NewUsers = lists:keydelete(UserPid, #user.pid, State#state.users),
            NewState = State#state{users = NewUsers},
            {reply, ok, NewState};
        _ -> {reply, {error, user_not_found}, State}
    end;
handle_call(get_users, _From, State) ->
    Users = lists:map(fun(#user{name = Name, pid = Pid}) -> {Name, Pid} end,
        State#state.users),
    {reply, Users, State};
handle_call(get_history, _From, State) ->
    Messages = lists:map(fun(#message{username = UserName, text = Text}) -> {UserName, Text} end,
        State#state.messages),
    OrderedMessages = lists:reverse(Messages),
    {reply, OrderedMessages, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
