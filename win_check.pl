% La fonction prend en entrée le talbeau des valeurs
% Il convertit le tableau en matrice
% Il calcule les transposés pour les colonnes et diagonales
% Il vérifie si une des lignes, colonnes ou diagonales contient 4 valeurs identiques



win(B, M) :-
    convert_board_vector_to_matrix(B, 6, 7, BM),
    print_matrix(BM),
    (   lignes_ok(M, BM)                      % Check rows
    ;   transpose(BM, TBM),                  % Check columns
        print_matrix(TBM),
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
