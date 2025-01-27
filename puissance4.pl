%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CST 381 -– Artificial Intelligence
%%% Robert Pinchbeck
%%% Final Project
%%% Due December 20, 2006
%%% Source : http://www.robertpinchbeck.com/college/work/prolog/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% A Prolog Implementation of Tic-Tac-Toe
%%% using the minimax strategy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

The following conventions are used in this program...

Single letter variables represent:

L - a list
N - a number, position, index, or counter
V - a value (usually a string)
A - an accumulator
H - the head of a list
T - the tail of a list

For this implementation, these single letter variables represent:

P - a player number (1 or 2)
B - the board (a 9 item list representing a 3x3 matrix)
    each "square" on the board can contain one of 3 values: x ,o, or e (for empty)
S - the number of a square on the board (1 - 9)
M - a mark on a square (x or o)
E - the mark used to represent an empty square ('e').
U - the utility value of a board position
R - a random number
D - the depth of the minimax search tree (for outputting utility values, and for debugging)

Variables with a numeric suffix represent a variable based on another variable.
(e.g. B2 is a new board position based on B)

For predicates, the last variable is usually the "return" value.
(e.g. opponent_mark(P,M), returns the opposing mark in variable M)

Predicates with a numeric suffix represent a "nested" predicate.

e.g. myrule2(...) is meant to be called from myrule(...)
     and myrule3(...) is meant to be called from myrule2(...)


There are only two assertions that are used in this implementation

asserta( board(B) ) - the current board
asserta( player(P, Type) ) - indicates which players are human/computer.

*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     FACTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_player(1, 2).      %%% determines the next player after the given player
next_player(2, 1).

inverse_mark(' x', ' o'). %%% determines the opposite of the given mark
inverse_mark(' o', ' x').

player_mark(1, ' x').    %%% the mark for the given player
player_mark(2, ' o').

opponent_mark(1, ' o').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, ' x').

blank_mark('e').        %%% the mark used in an empty square

maximizing(' x').        %%% the player playing x is always trying to maximize the utility of the board position
minimizing(' o').        %%% the player playing o is always trying to minimize the utility of the board position

corner_square(1, 1).    %%% map corner squares to board squares
corner_square(2, 7).
corner_square(3, 36).
corner_square(4, 42).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     MAIN PROGRAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run :-
    hello,          %%% Display welcome message, initialize game

    play(1),        %%% Play the game starting with player 1

    goodbye         %%% Display end of game message
    .

run :-
    goodbye
    .


hello :-
    initialize,
%    cls,
    nl,
    nl,
    nl,
    write('Welcome to Connect 4.'),
    read_players,
    output_players
    .

initialize :-
    random_seed,          %%% use current time to initialize random number generator
    blank_mark(E),
    asserta( board([E,E,E,E,E,E,E, E,E,E,E,E,E,E, E,E,E,E,E,E,E, E,E,E,E,E,E,E, E,E,E,E,E,E,E, E,E,E,E,E,E,E]) )  %%% create a blank board
    .

goodbye :-
    board(B),
    nl,
    nl,
    write('Game over: '),
    output_winner(B),
    retract(board(_)),
    retract(player(_,_)),
    read_play_again(V), !,
    (V == 'Y' ; V == 'y'),
    !,
    run
    .

read_play_again(V) :-
    nl,
    nl,
    write('Play again (Y/N)? '),
    read(V),
    (V == 'y' ; V == 'Y' ; V == 'n' ; V == 'N'), !
    .

read_play_again(V) :-
    nl,
    nl,
    write('Please enter Y or N.'),
    read_play_again(V)
    .


read_players :-
    nl,
    nl,
    write('Number of human players? '),
    read(N),
    set_players(N)
    .

set_players(0) :-
    asserta( player(1, computer) ),
    asserta( player(2, computer) ), !
    .

set_players(1) :-
    nl,
    write('Is human playing X or O (X moves first)? '),
    read(M),
    human_playing(M), !
    .

set_players(2) :-
    asserta( player(1, human) ),
    asserta( player(2, human) ), !
    .

set_players(N) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players
    .


human_playing(M) :-
    (M == 'x' ; M == 'X'),
    asserta( player(1, human) ),
    asserta( player(2, computer) ), !
    .

human_playing(M) :-
    (M == 'o' ; M == 'O'),
    asserta( player(1, computer) ),
    asserta( player(2, human) ), !
    .

human_playing(M) :-
    nl,
    write('Please enter X or O.'),
    set_players(1)
    .


play(P) :-
    board(B), !,
    output_board(B), !,
    not(game_over(P, B)), !,
    make_move(P, B), !,
    next_player(P, P2), !,
    play(P2), !
    .


%.......................................
% square
%.......................................
% The mark in a square(N) corresponds to an item in a list, as follows:

square(B, S, M) :-
    nth1(S, B, M).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WINNING CONDITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
win(B, M) :-
    convert_board_vector_to_matrix(B, 6, 7, BM),
    (   lignes_ok(M, BM)                      % Check rows
    ;   transpose(BM, TBM),                  % Check columns
        lignes_ok(M, TBM)
    ;   diagonal(BM, 4, Diagonals),          % Check diagonals
        lignes_ok(M, Diagonals)
    ).

% Convert board to matrix
convert_board_vector_to_matrix(B, R, C, BM) :-
    length(B, Length),
    Length =:= R * C,
    split_into_rows(B, C, BM).

% Split into rows
split_into_rows([], _, []).
split_into_rows(B, C, [R|T]) :-
    length(R, C),
    append(R, RestVector, B),
    split_into_rows(RestVector, C, T).

print_matrix([]).
print_matrix([H|T]) :- write(H), nl, print_matrix(T).

% Transpose
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose_1(F, [F|Fs], Ts).

transpose_1([], _, []).
transpose_1([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose_1(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

% Diagonal extraction
diagonal(Matrix, MinLength, Diagonals) :-
    diagonal1(Matrix, D1, MinLength),
    diagonal2(Matrix, D2, MinLength),
    append(D1, D2, Diagonals).

diagonal1(Matrix, Diagonals, MinLength) :-
    findall(D, (extract_diagonal_top_left_to_bottom_right(Matrix, D), length(D, L), L >= MinLength), Diagonals).

diagonal2(Matrix, Diagonals, MinLength) :-
    reverse(Matrix, Reversed),
    findall(D, (extract_diagonal_top_left_to_bottom_right(Reversed, D), length(D, L), L >= MinLength), Diagonals).


extract_diagonal_top_left_to_bottom_right(Matrix, Diagonal) :-
    nth0(RowStart, Matrix, _),
    extract_diagonal(RowStart, 0, Matrix, Diagonal).

extract_diagonal_top_left_to_bottom_right(Matrix, Diagonal) :-
    nth0(0, Matrix, FirstRow),
    nth0(ColStart, FirstRow, _),
    ColStart > 0,
    extract_diagonal(0, ColStart, Matrix, Diagonal).

extract_diagonal(Row, Col, Matrix, [Element|Rest]) :-
    nth0(Row, Matrix, CurrentRow),
    nth0(Col, CurrentRow, Element),
    Row1 is Row + 1,
    Col1 is Col + 1,
    extract_diagonal(Row1, Col1, Matrix, Rest).

extract_diagonal(Row, Col, Matrix, []) :-
    ( \+ nth0(Row, Matrix, _)
    ; nth0(Row, Matrix, CurrentRow),
      \+ nth0(Col, CurrentRow, _)
    ).

% Check rows or diagonals for identical values
ligne_ok(V, [V, V, V, V | _]).
ligne_ok(V, [_ | Q]) :- ligne_ok(V, Q).

lignes_ok(V, [Ligne | _]) :- ligne_ok(V, Ligne).
lignes_ok(V, [_ | Reste]) :- lignes_ok(V, Reste).


%.......................................
% move
%.......................................
% applies a move on the given board
% (put mark M in square S on board B and return the resulting board B2)
%

move(B,S,M,B2) :-
    set_item(B,S,M,B2)
    .


%.......................................
% game_over
%.......................................
% determines when the game is over
%
game_over(P, B) :-
    game_over2(P, B)
    .

game_over2(P, B) :-
    opponent_mark(P, M),   %%% game is over if opponent wins
    win(B, M)
    .

game_over2(P, B) :-
    blank_mark(E),
    not(square(B,S,E))     %%% game is over if opponent wins
    .


%.......................................
% make_move
%.......................................
% requests next move from human/computer,
% then applies that move to the given board
%

make_move(P, B) :-
    player(P, Type),

    make_move2(Type, P, B, B2),

    retract( board(_) ),
    asserta( board(B2) )
    .

valid(B,S,E) :-
    square(B,S,E),
    Below is S + 7,
    not(square(B,Below,E)).

make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Player '),
    write(P),
    write(' move? '),
    read(S),
    blank_mark(E),
    valid(B,S,E),
    player_mark(P, M),
    move(B, S, M, B2), !
    .

make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Please select a valid entry.'),
    make_move2(human,P,B,B2)
    .

make_move2(computer, P, B, B2) :-
    nl,
    nl,
    write('Computer is thinking about next move...'),
    player_mark(P, M),
    minimax(0, B, M, S, U, 4),
    move(B,S,M,B2),

    nl,
    nl,
    write('Computer places '),
    write(M),
    write(' in square '),
    write(S),
    write('.')
    .


%.......................................
% moves
%.......................................
% retrieves a list of available moves (empty squares) on a board.
%

% Base case: when N is greater than 42, the list of valid moves is empty
find_valid_moves(_, N, _, []) :- N > 42, !.

% Recursive case: check if the move is valid and add it to the list if it is
find_valid_moves(B, N, E, [N|L]) :-
    N =< 42,
    valid(B, N, E), !,
    N1 is N + 1,
    find_valid_moves(B, N1, E, L).

% Recursive case: skip the move if it is not valid
find_valid_moves(B, N, E, L) :-
    N =< 42,
    N1 is N + 1,
    find_valid_moves(B, N1, E, L).

moves(B,L) :-
    not(win(B,x)),                %%% if either player already won, then there are no available moves
    not(win(B,o)),
    blank_mark(E),
    find_valid_moves(B,1,E,L),
    L \= []
    .


%.......................................
% utility
%.......................................
% determines the value of a given board position
%

%utility(B,U) :-
%    win(B,' x'),
%    U = 1,
%    !
%    .

%utility(B,U) :-
%    win(B,' o'),
%    U = (-1),
%    !
%    .

%utility(B,U) :-
%    U = 0
%    .


%.......................................
% minimax
%.......................................
% The minimax algorithm always assumes an optimal opponent.
% For tic-tac-toe, optimal play will always result in a tie, so the algorithm is effectively playing not-to-lose.

% For the opening move against an optimal player, the best minimax can ever hope for is a tie.
% So, technically speaking, any opening move is acceptable.
% Save the user the trouble of waiting  for the computer to search the entire minimax tree
% by simply selecting a random square.

minimax(D,[E,E,E, E,E,E, E,E,E, E,E,E, E,E,E, E,E,E,E,E, E,E,E, E,E,E, E,E,E, E,E,E, E,E,E,E,E, E,E,E, E,E],M,S,U,Limit) :-
    blank_mark(E),
    random_int_1n(3,S1),
    S is S1 + 37,
    !
    .

minimax(D,B,M,S,U,Limit) :-
    D2 is D + 1,
    moves(B,L),          %%% get the list of available moves
    %write(D2),
    %write(L)
    %,nl,
    !,
    evaluate(D2,B,M,L,S,U,Limit),  %%% recursively determine the best available move
    !
    .

% if there are no more available moves,
% then the minimax value is the utility of the given board position

minimax(D,B,M,S,U,Limit) :-
    utility(B,U)
    .


%.......................................
% best
%.......................................
% determines the best move in a given list of moves by recursively calling minimax
%

% if there is only one move left in the list...

best(D,B,M,[S1],S,U) :-
    move(B,S1,M,B2),        %%% apply that move to the board,
    inverse_mark(M,M2),
    !,
    minimax(D,B2,M2,_S,U),  %%% then recursively search for the utility value of that move.
    S = S1, !,
    output_value(D,S,U),
    !
    .

% if there is more than one move in the list...

best(D,B,M,[S1|T],S,U) :-
    move(B,S1,M,B2),             %%% apply the first move (in the list) to the board,
    inverse_mark(M,M2),
    !,
    minimax(D,B2,M2,_S,U1),      %%% recursively search for the utility value of that move,
    best(D,B,M,T,S2,U2),         %%% determine the best move of the remaining moves,
    output_value(D,S1,U1),
    better(D,M,S1,U1,S2,U2,S,U)  %%% and choose the better of the two moves (based on their respective utility values)
    .


%.......................................
% evaluate
%.......................................
% returns the evaluation of the board.

% Heuristic evaluation function for the Connect 4 board
utility(B, Score) :-   
    count_aligned(B, ' x', 4, FourCrossInARow),
    count_aligned(B, ' x', 3, ThreeCrossInARow),
    count_aligned(B, ' x', 2, TwoCrossInARow),
    center_control(B, ' x', CrossCenterControl),
    count_aligned(B, ' o', 4, FourCircleInARow),
    count_aligned(B, ' o', 3, ThreeCircleInARow),
    count_aligned(B, ' o', 2, TwoCircleInARow),
    center_control(B, ' o', CircleCenterControl),
    Score is FourCrossInARow * 1000 - FourCircleInARow * 1000 + ThreeCrossInARow * 100 - ThreeCircleInARow * 100 + TwoCrossInARow * 10 - TwoCircleInARow * 10 + CrossCenterControl - CircleCenterControl .

% Count the number of aligned sequences of a given length for a player
count_aligned(B, Player, Length, Count) :-
    findall(_, aligned_sequence(B, Player, Length), Sequences),
    length(Sequences, Count).

% Check for aligned sequences of a given length
aligned_sequence(B, Player, Length) :-
    horizontal_sequence(B, Player, Length);
    vertical_sequence(B, Player, Length);
    diagonal_sequence(B, Player, Length).

% Check for horizontal sequences
horizontal_sequence(B, Player, Length) :-
    between(0, 5, Row),
    Start is Row * 7 +1,
    End is Start + 7 +1 - Length,
    between(Start, End, Index),
    check_sequence(B, Player, Index, 1, Length).

% Check for vertical sequences
vertical_sequence(B, Player, Length) :-
    End is 42 - (Length-1) * 7,
    between(1, End, Index),
    check_sequence(B, Player, Index, 7, Length).

% Check for diagonal sequences (bottom-left to top-right)
diagonal_sequence(B, Player, Length) :-
    between(0, 5, Row),
    between(1, 7, Col),
    Index is Row * 7 + Col,
    check_sequence(B, Player, Index, 6, Length).

% Check for diagonal sequences (top-left to bottom-right)
diagonal_sequence(B, Player, Length) :-
    between(0, 5, Row),
    between(1, 7, Col),
    Index is Row * 7 + Col,
    check_sequence(B, Player, Index, 8, Length).


% Check if there is a sequence of a given length starting from an index with a given step
check_sequence(B, Player, Index, Step, Length) :-
    End is Index + Step * (Length - 1),
    End < 43,
    check_sequence_helper(B, Player, Index, Step, Length).

check_sequence_helper(_, _, _, _, 0).
check_sequence_helper(B, Player, Index, Step, Length) :-
    square(B, Index, Player),
    NextIndex is Index + Step,
    NextLength is Length - 1,
    check_sequence_helper(B, Player, NextIndex, Step, NextLength).

% Calculate center control score
center_control(B, Player, Score) :-
    findall(Index, (between(0, 5, Row), between(3, 5, Col), Index is Row * 7 + Col, square(B, Index, Player)), CenterPieces),
    length(CenterPieces, Score).


evaluate(D, B, M, S, U) :-
    utility(B, U)
    .

evaluate(D,B,M,[S1],S,U,Limit) :- %%% one possible move
    not(D==Limit),
    move(B,S1,M,B2),        %%% apply that move to the board,
    inverse_mark(M,M2),     %%% change player turn
    !,
    minimax(D,B2,M2,_S,U,Limit),      %%% recursively search for the utility value of that move,
    S = S1,
    output_value(D, S, U),
    !
    .

evaluate(Limit,B,M,[S1],S,U,Limit) :- %%% one possible move case occurence limit
    move(B,S1,M,B2),        %%% apply that move to the board,
    evaluate(Limit,B2,M,S1,U),  %%% then evaluate the board position
    S = S1, !
    .

evaluate(Limit,B,M,[S1|T],S,U,Limit) :- %%% multiple possible moves case occurence limit
    move(B,S1,M,B2),             %%% apply the first move (in the list) to the board,
    evaluate(Limit,B2,M,S1,U1),      %%% then evaluate the board position,
    evaluate(Limit,B,M,T,S2,U2,Limit),         %%% determine the best move of the remaining moves,
    better(Limit,M,S1,U1,S2,U2,S,U)  %%% and choose the better of the two moves (based on their respective utility values)
    .

evaluate(D,B,M,[S1|T],S,U,Limit) :-    %%% multiple possible moves
    not(D==Limit),
    move(B,S1,M,B2),             %%% apply the first move (in the list) to the board,
    inverse_mark(M,M2),          %%% change player turn
    !,
    minimax(D,B2,M2,_S,U1,Limit),      %%% recursively search for the utility value of that move,
    evaluate(D,B,M,T,S2,U2,Limit),     %%% determine the best move of the remaining moves,
    output_value(D, S1, U1),         
    better(D,M,S1,U1,S2,U2,S,U)  %%% and choose the better of the two moves (based on their respective utility values)
    .

%.......................................
% better
%.......................................
% returns the better of two moves based on their respective utility values.
%
% if both moves have the same utility value, then one is chosen at random.

better(D,M,S1,U1,S2,U2,     S,U) :-
    maximizing(M),                     %%% if the player is maximizing
    U1 > U2,                           %%% then greater is better.
    S = S1,
    U = U1,
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-
    minimizing(M),                     %%% if the player is minimizing,
    U1 < U2,                           %%% then lesser is better.
    S = S1,
    U = U1,
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-
    U1 == U2,                          %%% if moves have equal utility,
    random_int_1n(10,R),               %%% then pick one of them at random
    better2(D,R,M,S1,U1,S2,U2,S,U),
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-        %%% otherwise, second move is better
    S = S2,
    U = U2,
    !
    .


%.......................................
% better2
%.......................................
% randomly selects two squares of the same utility value given a single probability
%

better2(D,R,M,S1,U1,S2,U2,  S,U) :-
    R < 6,
    S = S1,
    U = U1,
    !
    .

better2(D,R,M,S1,U1,S2,U2,  S,U) :-
    S = S2,
    U = U2,
    !
    .



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_players :-
    nl,
    player(1, V1),
    write('Player 1 is '),   %%% either human or computer
    write(V1),

    nl,
    player(2, V2),
    write('Player 2 is '),   %%% either human or computer
    write(V2),
    !
    .


output_winner(B) :-
    win(B, ' x'),
    write('X wins.'), nl.

output_winner(B) :-
    win(B, ' o'),
    write('O wins.'), nl.

output_winner(_) :-
    write('No winner.'), nl.



output_board(B) :-
    nl,
    nl,
    
    forall(
        between(1, 6, I), 
        (
            forall(
                between(1, 7, J), 
                (
                    Index is 7 * (I - 1) + J,
                    output_square(B, Index),
                    write('|')
                )
            ), 
            nl,
            write('-----------------------------------'),
            nl
        )
    ), !
    .

output_board :-
    board(B),
    output_board(B), !
    .

output_square(B,S) :-
    square(B,S,M),
    write(' '),
    output_square2(S,M),
    write(' '), !
    .

output_square2(S, E) :-
    blank_mark(E),
    format('~|~`0t~d~2+', S), !
    .

output_square2(S, M) :-
    write(M), !              %%% if square is marked, output the mark
    .

output_value(D,S,U) :-
    D == 1,
    nl,
    write('Square '),
    write(S),
    write(', utility: '),
    write(U), !
    .

output_value(D,S,U) :-
    true
    .



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PSEUDO-RANDOM NUMBERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%.......................................
% random_seed
%.......................................
% Initialize the random number generator...
% If no seed is provided, use the current time
%

random_seed :-
    random_seed(_),
    !
    .

random_seed(N) :-
    nonvar(N),
% Do nothing, SWI-Prolog does not support seeding the random number generator
    !
    .

random_seed(N) :-
    var(N),
% Do nothing, SWI-Prolog does not support seeding the random number generator
    !
    .

/*****************************************
 OTHER COMPILER SUPPORT
******************************************

arity_prolog___random_seed(N) :-
    nonvar(N),
    randomize(N),
    !
    .

arity_prolog___random_seed(N) :-
    var(N),
    time(time(Hour,Minute,Second,Tick)),
    N is ( (Hour+1) * (Minute+1) * (Second+1) * (Tick+1)),
    randomize(N),
    !
    .

******************************************/



%.......................................
% random_int_1n
%.......................................
% returns a random integer from 1 to N
%
random_int_1n(N, V) :-
    V is random(N) + 1,
    !
    .

/*****************************************
 OTHER COMPILER SUPPORT
******************************************

arity_prolog___random_int_1n(N, V) :-
    R is random,
    V2 is (R * N) - 0.5,
    float_text(V2,V3,fixed(0)),
    int_text(V4,V3),
    V is V4 + 1,
    !
    .

******************************************/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LIST PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member([V|T], V).
member([_|T], V) :- member(T,V).



%.......................................
% set_item
%.......................................
% Given a list L, replace the item at position N with V
% return the new list in list L2
%

set_item(L, N, V, L2) :-
    set_item2(L, N, V, 1, L2)
        .

set_item2( [], N, V, A, L2) :-
    N == -1,
    L2 = []
    .

set_item2( [_|T1], N, V, A, [V|T2] ) :-
    A = N,
    A1 is N + 1,
    set_item2( T1, -1, V, A1, T2 )
    .

set_item2( [H|T1], N, V, A, [H|T2] ) :-
    A1 is A + 1,
    set_item2( T1, N, V, A1, T2 )
    .


%.......................................
% get_item
%.......................................
% Given a list L, retrieve the item at position N and return it as value V
%

get_item(L, N, V) :-
    get_item2(L, N, 1, V)
    .

get_item2( [], _N, _A, V) :-
    V = [], !,
    fail
        .

get_item2( [H|_T], N, A, V) :-
    A = N,
    V = H
    .

get_item2( [_|T], N, A, V) :-
    A1 is A + 1,
    get_item2( T, N, A1, V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
