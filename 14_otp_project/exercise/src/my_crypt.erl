-module(my_crypt).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_key/0, set_key/1, encode/1, hash/1]).

-define(SEED, {13, 45, 77}).

-record(state, {
    crypt_key,
    hash_table
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

get_key() ->
    gen_server:call(?MODULE, get_key).

set_key(Key) ->
    gen_server:call(?MODULE, {set_key, Key}).

encode(Bin) ->
    gen_server:call(?MODULE, {encode, Bin}).

hash(Bin) ->
    gen_server:call(?MODULE, {hash, Bin}).

hash8(Bin, HashTable) ->
    {ok, HashSize} = application:get_env(hash_size),
    A = lists:map(fun
        (I) ->
            Hash = lists:foldl(fun
                (C, CurrentHash) ->
                    lists:nth((CurrentHash bxor C) + 1, HashTable)
                end,
                I,  binary:bin_to_list(Bin)),
            int2bin(Hash)
        end,
        lists:seq(1, HashSize div 2)),
    unicode:characters_to_binary(A).

int2bin(Int) when Int < 16 -> <<"0", (integer_to_binary(Int, 16))/binary>>;
int2bin(Int) -> integer_to_binary(Int, 16).


encode(Bin, Key) ->
    encode(Bin, Key, {Key, []}).
encode(<<>>, _, {_, EncodedList}) ->
    binary:list_to_bin(lists:reverse(EncodedList));
encode(Bin = <<_BinHead:8, _BinRest/binary>>, <<>>, {Key, EncodedList}) ->
    encode(Bin, Key, {Key, EncodedList});
encode(<<BinHead:8, BinRest/binary>>, <<KeyHead:8, KeyRest/binary>>, {Key, EncodedList}) ->
    encode(BinRest, KeyRest,
        {Key, [BinHead bxor KeyHead | EncodedList]}).

randomize(List) ->
    D = lists:map(fun(A) -> 
              {rand:uniform(), A}
          end, List),

    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.

get_hash_table() ->
    MonotoneTable = lists:seq(0, 255),
    randomize(MonotoneTable).


%%% gen_server API

init(no_args) ->
    {ok, EncodeKey} = application:get_env(crypt_key),
    rand:seed(exs1024, ?SEED),
    HashTable = get_hash_table(),
    State = #state{
        crypt_key = EncodeKey,
        hash_table = HashTable},
    {ok, State}.

handle_call(get_key, _From, State) ->
    {reply, State#state.crypt_key, State};
handle_call({set_key, Key = <<_Bin/binary>>}, _From, State) ->
    {reply, ok, State#state{crypt_key = Key}};
handle_call({encode, Bin}, _From, State) ->
    Encoded = encode(Bin, State#state.crypt_key),
    {reply, Encoded, State};
handle_call({hash, Bin}, _From, State) ->
    Hashed = hash8(Bin, State#state.hash_table),
    {reply, Hashed, State}.

handle_cast(_Request, #state{} = State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.