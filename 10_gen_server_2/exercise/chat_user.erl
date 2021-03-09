-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(message, {
        username :: binary(),
        text :: binary()
    }).
-record(state, {
        messages :: []
    }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

add_message(UserPid, UserName, Text) ->
    gen_server:cast(UserPid, {add_message, UserName, Text}),
    ok.

get_messages(UserPid) ->
    gen_server:call(UserPid, get_messages).

init(_Args) ->
    {ok, #state{messages = []}}.

handle_cast({add_message, UserName, Text}, State) ->
    NewMessage = #message{username = UserName, text = Text},
    NewMessages = [NewMessage | State#state.messages],
    {noreply, #state{messages = NewMessages}}.

handle_call(get_messages, _From, State) ->
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