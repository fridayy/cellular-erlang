-module(cerll).

%-export([]).
-compile(export_all).

%% At each step in time, the following transitions occur:
%%
%%   Any live cell with fewer than two live neighbours dies, as if by underpopulation.
%%   Any live cell with two or three live neighbours lives on to the next generation.
%%   Any live cell with more than three live neighbours dies, as if by overpopulation.
%%   Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
%%
%% These rules, which compare the behavior of the automaton to real life, can be condensed into the following:
%%
%%   Any live cell with two or three live neighbours survives.
%%   Any dead cell with three live neighbours becomes a live cell.
%%   All other live cells die in the next generation. Similarly, all other dead cells stay dead.

test() -> 
    test1(),
    test2(),
    test3(),
    test4(),
    test5(),
    test6().

test1() -> 
    B = bitmap(8,8),
    NewB = set(B, 4,2),
    {ok, 1} = get(NewB, 4,2).

test2() -> 
    B = bitmap(8,8),
    B0 = set(B, 4,2),
    1 = foldr(B0, 0, fun ({_X,_Y, S},Acc) -> S + Acc end).

test3() ->
    B = bitmap(8,8),    
    B0 = set(B, 6,5),
    R = foldr(B0, [], fun ({X,Y, S}, Acc) -> 
            case S of
                1 -> 
                    [{X,Y, alive} | Acc];
                _ -> Acc
            end
        end),
    [{6,5, alive}] = R.

test4() -> 
    B = bitmap(8,8),
    B0 = set(B, 4, 3),
    B1 = set(B0, 5,3),
    B2 = set(B1, 4,4),
    2 = get_neighbors(B2, {4,3}).

test5() ->
    B = bitmap(8,8),
    B0 = set(B, 4,3),
    {ok, 1} = get(B0, 4,3),
    B1 = set(B0, 4,3),
    {ok, 0} = get(B1, 4,3).

test6() -> 
    B = bitmap(8,8),
    B0 = set(B, 4, 3),
    B1 = set(B0, 5,3),
    B2 = set(B1, 4,4),
    render(B2),
    io:format("~n~n"),
    B3 = tick(B2),
    render(B3),
    io:format("~n~n"),
    B4 = tick(B3),
    render(B4).

osc() -> 
    B = bitmap(16,16),
    BB = set_all(B, [
          {8,8},
          {9,8},
          {7,8},
          {8,7},
          {8,9}
        ]),
    play(BB).

glider() ->
    play(set_all(bitmap(32,32), [
        {8,8},
        {9,8},
        {10,8},
        {8,9},
        {11,10}
        ]), 50).


play(B, Timeout) ->
    timer:sleep(Timeout),
    io:format("~n~n"),
    render(B),
    play(tick(B)).

play(B) ->
    play(B, 1000).

tick(B) -> 
    foldr(B, B, fun(Cell, Acc) -> 
            case next_state(B, Cell) of
                {change, {X,Y, _S}} -> 
                    set(Acc, X, Y);
                {no_change, _} -> Acc
            end
        end).

%computes the next state of a cell
next_state(B, {X,Y,1} = Cell) ->
    case get_neighbors(B, {X,Y}) of
        2 -> {no_change, Cell};
        3 -> {no_change, Cell};
        _ -> {change, {X,Y,0}} %the cell dies
    end;

next_state(B, {X,Y, 0} = Cell) ->
    case get_neighbors(B, {X,Y}) of
        3 -> {change, {X,Y, 1}};
        _ -> {no_change, Cell} %still dead :(            
    end.

%% Returns the sum of the states off all neighboring cells
%%     o-o-o  [(Y+1) = U, (Y+1,X-1) = UL, (Y+1,X+1) = UR]
%%     o-x-o  [(X-1) = L, (X+1) = R]
%%     o-o-o  [(Y-1) = D, (Y-1,X-1) = DL, (Y-1,X+1) = DR]
get_neighbors(Bitmap, {X,Y}) ->
    U = get_strict(Bitmap, X, Y+1),
    UL = get_strict(Bitmap, X-1, Y+1),
    UR = get_strict(Bitmap, X+1, Y+1),
    L = get_strict(Bitmap, X-1, Y),
    R = get_strict(Bitmap, X+1, Y),
    D = get_strict(Bitmap, X, Y-1),
    DL = get_strict(Bitmap, X-1,Y-1),
    DR = get_strict(Bitmap, X+1, Y-1),
    U + UL + UR + L + R+ D+ DL + DR.


%% if a cell considered out of bounds it is threaded as dead cell
get_strict(Bitmap, X, Y) ->
    case get(Bitmap, X, Y) of
        {ok, Value} -> Value;
        {err, _} -> 0
    end.

foldr(Bitmap, Acc, F) ->
    do_foldr(Bitmap, 0,0, Acc, F).

%% a cell is {X,Y,S}
do_foldr(#{rows := Rows, cols := Cols, bitmap := _Bitmap} = B, I, J, Acc, F) when (I < Cols) and (J < Rows) ->
    {ok, Value} = get(B,I,J),
    F({I, J, Value}, do_foldr(B, I + 1, J, Acc, F));

do_foldr(#{rows := Rows, cols := _Cols, bitmap := _Bitmap} = B, _I, J, Acc, F) when J < Rows->
      {ok, Value} = get(B,0,J),
      F({0, J , Value}, do_foldr(B, 0, J + 1, Acc, F));

do_foldr(_Bitmap, _I, _J, Acc, _F) ->
    Acc.


bitmap(N, M) -> 
    Dimension = N * M,
    #{rows => N, cols => M, bitmap => <<0:Dimension>>}.

get(#{rows := Rows, cols := Cols, bitmap := Bitmap}, X, Y) when (X < Cols) and (Y < Rows) and (X >= 0) and (Y >= 0) ->
    Before = Y * Cols + X,
    <<_B:Before, Element:1, _Rest/bitstring>> = Bitmap,
    {ok, Element};

get(_B, _X, _Y) ->
    {err, out_of_bounds}.

set(#{rows := Rows, cols := Cols, bitmap := Bitmap}, X, Y) ->
    Total = Rows * Cols,
    Before = Y * Cols + X,
    After = Total - Before - 1,
    <<B:Before, Element:1, Rest:After>> = Bitmap,
    if
        Element =:= 1 ->
            #{rows => Rows, cols => Cols, bitmap => <<B:Before, 0:1, Rest: After>>};
        true ->
             #{rows => Rows, cols => Cols, bitmap => <<B:Before, 1:1, Rest: After>>}
    end.

set_all(Bitmap, Cells) ->
    lists:foldl(fun({X,Y}, Acc) -> 
        set(Acc, X,Y)
    end, 
    Bitmap,                
    Cells).

render(#{rows := Rows} = Bitmap) ->
    do_render(Bitmap, (Rows - 1)).

do_render(#{rows := Rows, cols := _Cols, bitmap := _Bitmap} = B, CurrentRow) when CurrentRow =:= 0 ->
    io:format("~p~n", [row_as_list(B, CurrentRow, (Rows - 1), "")]);

do_render(#{rows := Rows, cols := _Cols, bitmap := _Bitmap} = B, CurrentRow) ->
    R = row_as_list(B, CurrentRow, (Rows - 1), ""),
    io:format("~p~n", [R]),
    do_render(B, CurrentRow - 1).


row_as_list(Bitmap, Row, 0, Acc) ->
    {ok, Value} = get(Bitmap, 0, Row),
    % [Value | Acc];
    to_string(Value) ++ Acc;

row_as_list(Bitmap, Row, X, Acc) ->
    % io:format("Row:~p X:~p~n", [Row,X]),
    {ok, Value} = get(Bitmap, X, Row),
    % row_as_list(Bitmap, Row, X - 1, [Value | Acc]).
    row_as_list(Bitmap, Row, X - 1, to_string(Value) ++ Acc).

to_string(Value) ->
    case Value of 
        0 -> "   ";
        1 -> " X "
    end.