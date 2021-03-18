-module(my_crypt_worker).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_key/0, set_key/1, encode/1]).

-record(state, {
    encode_key
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

get_key() ->
    gen_server:call(?MODULE, get_key).

set_key(Key) ->
    gen_server:call(?MODULE, {set_key, Key}).

encode(Bin) ->
    gen_server:call(?MODULE, {encode, Bin}).

encode(Bin, Key) ->
    encode(Bin, Key, {Key, []}).
encode(<<>>, _, {_, EncodedList}) ->
    binary:list_to_bin(lists:reverse(EncodedList));
encode(Bin = <<_BinHead:8, _BinRest/binary>>, <<>>, {Key, EncodedList}) ->
    encode(Bin, Key, {Key, EncodedList});
encode(<<BinHead:8, BinRest/binary>>, <<KeyHead:8, KeyRest/binary>>, {Key, EncodedList}) ->
    encode(BinRest, KeyRest,
        {Key, [BinHead bxor KeyHead | EncodedList]}).

%%% gen_server API

init(no_args) ->
    {ok, EncodeKey} = application:get_env(encode_key),
    State = #state{encode_key = EncodeKey},
    {ok, State}.

% handle_call(_Request, _From, #state{} = State) ->
%     {reply, ok, State}.
handle_call(get_key, _From, State) ->
    {reply, State#state.encode_key, State};
handle_call({set_key, Key = <<_Bin/binary>>}, _From, State) ->
    {reply, ok, State#state{encode_key = Key}};
handle_call({encode, Bin}, _From, State) ->
    Encoded = encode(Bin, State#state.encode_key),
    {reply, Encoded, State}.

handle_cast(_Request, #state{} = State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.