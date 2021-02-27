-module(tic_tac_toe).

-export([move/3, new_game/0, win/1]).

new_game() -> {{f, f, f}, {f, f, f}, {f, f, f}}.

win(GameState) ->
    Result = case GameState of
      {{X, X, X}, _, _} -> X;
      {_, {X, X, X}, _} -> X;
      {_, _, {X, X, X}} -> X;
      {{X, _, _},
      {X, _, _},
      {X, _, _}} -> X;
      {{_, X, _},
      {_, X, _},
      {_, X, _}} -> X;
      {{_, _, X},
      {_, _, X},
      {_, _, X}} -> X;
      {{X, _, _},
      {_, X, _},
      {_, _, X}} -> X;
      {{_, _, X},
      {_, X, _},
      {X, _, _}} -> X;
      _ -> false 
    end,
    case Result of
        f -> no_win;
        false -> no_win;
        Win -> {win, Win}
    end.
        

move(Cell, _, _) when (Cell < 1) or (Cell > 9) ->
    {error, invalid_move};
move(Cell, Player, GameState) ->
    Row = 1 + (Cell - 1) div 3,
    Col = 1 + (Cell - 1) rem 3,
    SelectedRow = element(Row, GameState),
    SelectedCell = element(Col, SelectedRow),
    case SelectedCell of
        f -> {ok,
                setelement(Row, GameState,
                    setelement(Col, SelectedRow, Player))};
        _ -> {error, invalid_move}
    end.
