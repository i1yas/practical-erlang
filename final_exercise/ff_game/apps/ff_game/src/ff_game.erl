-module(ff_game).

-export([test/0]).
-export([initial_field/0
        ,initial_field/2
        ,randomize_field/2
        ,move/3
        ,get_player_statuses/1
        ,find_player/2
        ,draw_field/1]).

-type(player() :: player_a | player_b).
-type(move() :: left | rigth | up | down).
-type(cell() :: stable | fallen | player_a | player_b).
-type(row() :: [cell()]).
-type(field() :: [row()]).
-type(size() :: 5..500).
-type(position() :: {pos_integer(), pos_integer()}).


test() ->
    F = initial_field(),
    {ok, F2} = move(player_a, left, F),
    B = draw_field(F2),
    io:format("~s", [B]),
    ok.


%%% Module API

-spec initial_field() -> field().
initial_field() ->
    initial_field(10, 10).

-spec randomize_field(field(), {integer(), integer()}) -> field().
randomize_field(Field, {W, H}) ->
    MZ = matrix_zipper:from_matrix(Field),
    RandomizedMZ = randomize_field(MZ, {W, H}, 10),
    matrix_zipper:to_matrix(RandomizedMZ).

-spec randomize_field(matrix_zipper:mz(), {integer(), integer()}, integer()) -> matrix_zipper:mz().
randomize_field(MZ, _, 0) -> MZ;
randomize_field(MZ, {W, H}, Count) ->
    X = rand:uniform(W),
    Y = rand:uniform(H),
    case erlz:error_do(MZ, [
        fun(M) -> matrix_zipper:right(M, X) end,
        fun(M) -> matrix_zipper:down(M, Y) end,
        fun(M) -> case matrix_zipper:get(M) of
                stable -> {ok, M};
                _ -> {error, cant_fall_this}
            end
        end,
        fun(M) -> {ok, matrix_zipper:set(M, fallen)} end
    ]) of
        {ok, NewMZ} -> randomize_field(NewMZ, {W, H}, Count - 1);
        {error, _} -> randomize_field(MZ, {W, H}, Count)
    end.


-spec initial_field(size(), size()) -> field().
initial_field(W, H) when W >= 5 andalso H >= 5 ->
    [first_row(W) |
     [row(W) || _ <- lists:seq(1, H - 2)]
     ++ [last_row(W)]].

-spec move(player(), move(), field()) -> {ok, field()} | {error, invalid_move}.
move(Player, Move, Field) ->
    {ok, MZ} = matrix_zipper:find(matrix_zipper:from_matrix(Field), Player),
    % check for unstable or player
    MoveResult = erlz:error_do(MZ, [
        fun (M) -> {ok, matrix_zipper:set(M, fallen)} end,
        fun matrix_zipper:Move/1,
        fun (M) -> case matrix_zipper:get(M) of
                stable -> {ok, M};
                NotFree -> {error, NotFree}
            end
        end,
        fun (M) -> {ok, matrix_zipper:set(M, Player)} end
    ]),
    case MoveResult of
        {ok, NewMZ} ->
            {ok, matrix_zipper:to_matrix(NewMZ)};
        {error, _Reason} ->
            {error, invalid_move} 
    end.

-spec get_player_statuses(field()) -> #{player_a => boolean(), player_b => boolean()}.
get_player_statuses(Field) ->
    SidesA = [
        move(player_a, right, Field),
        move(player_a, left, Field),
        move(player_a, up, Field),
        move(player_a, down, Field)
    ],
    SidesB = [
        move(player_b, right, Field),
        move(player_b, left, Field),
        move(player_b, up, Field),
        move(player_b, down, Field)
    ],
    FreeSides = fun(Sides) -> lists:foldl(fun
        ({ok, _}, Count) -> Count + 1;
        (_, Count) -> Count
    end, 0, Sides) end,
    #{player_a => FreeSides(SidesA) > 0, player_b => FreeSides(SidesB) > 0}.


-spec find_player(player(), field()) -> position().
find_player(Player, Field) ->
    lists:foldl(fun(_, {X,Y}) -> {X,Y};
                   (Row, RowNum) ->
                        case find_player_in_row(Player, Row) of
                            not_found -> RowNum + 1;
                            ColumnNum -> {RowNum, ColumnNum}
                        end
                end,
                1, Field).


-spec draw_field(field()) -> binary().
draw_field(Field) ->
    unicode:characters_to_binary(
      [
       draw_underline(length(hd(Field))),
       lists:map(fun draw_row/1, Field)
      ]).


%%% Inner Functions

-spec first_row(size()) -> row().
first_row(Size) ->
    [player_a | row(Size - 1)].


-spec last_row(size()) -> row().
last_row(Size) ->
    row(Size - 1) ++ [player_b].


-spec row(size()) -> row().
row(Size) ->
    [stable || _ <- lists:seq(1, Size)].


-spec find_player_in_row(player(), row()) -> pos_integer() | not_found.
find_player_in_row(Player, Row) ->
    NoPlayer = lists:takewhile(fun(Cell) -> Cell =/= Player end, Row),
    case NoPlayer of
        Row -> not_found;
        _ -> length(NoPlayer) + 1
    end.


-spec draw_row(row()) -> iolist().
draw_row(Row) ->
    R = lists:map(fun draw_cell/1, Row),
    U = draw_underline(length(R)),
    [R, "|\r\n", U].


-spec draw_underline(size()) -> iolist().
draw_underline(Size) ->
    [[" ---" || _ <- lists:seq(1, Size)], "\r\n"].


-spec draw_cell(cell()) -> iolist().
draw_cell(stable)   -> "|   ";
draw_cell(fallen)   -> "| x ";
draw_cell(player_a) -> "| A ";
draw_cell(player_b) -> "| B ".
