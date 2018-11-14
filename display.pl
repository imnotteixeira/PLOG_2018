display_game(Board, Player):-
    nl,
    length(Board, Length),
    gen_column_labels(Length, Labels, 65),
    write(' '), display_column_labels(Labels, '', ' '),
    display_separator,
    display_matrix(Board, 0),
    write('Player '),
    write(Player),
    write('\'s turn.'), nl.

%displays a matrix representing a board
display_matrix([], _).
display_matrix([H | T], LineNumber):-
    display_separated_line(H, LineNumber, '|'),
    display_separator,
    LineNumber1 is LineNumber+1,
    display_matrix(T, LineNumber1).

%displays a line with vertical separators
display_separated_line([], LineNumber, SeparatorStr):- write(SeparatorStr), write('  '), write(LineNumber), nl.
display_separated_line([H | T], LineNumber, SeparatorStr):-
    print_cell(H, SeparatorStr),
    display_separated_line(T, LineNumber, SeparatorStr).

%displays a line with column labels with vertical separators
display_column_labels([], LineNumber, SeparatorStr):- write(SeparatorStr), write('  '), write(LineNumber), nl.
display_column_labels([H | T], LineNumber, SeparatorStr):-
    print_cell_no_translate(H, SeparatorStr),
    display_column_labels(T, LineNumber, SeparatorStr).

%displays a line without separators
display_line([]):- nl.
display_line([H | T]):-
    traducao(H, X),
    put_code(X),
    display_line(T).

%prints a non-translated cell
print_cell_no_translate(C, SeparatorStr):-
    write(SeparatorStr),
    write(' '),
    put_code(C),
    write('  ').

%prints a translated cell
print_cell(C, SeparatorStr):-
    write(SeparatorStr),
    traducao(C, X),
    write(' '),
    put_code(X),
    write('  ').


%prints an horizontal separator
display_separator:-
    gen_line(56, SeperatorLineList, -1),
    display_line(SeperatorLineList).
    
%generates the column labels
gen_column_labels(0, [], _).
gen_column_labels(Size, List, LabelNr):-
    Size1 is Size - 1,
    NextLabelNr is LabelNr + 1,
    gen_column_labels(Size1, List1, NextLabelNr),
    List = [LabelNr | List1].

%generates a line of n given chars
gen_line(0, [], _).
gen_line(Size, List, Value):-
    Size1 is Size - 1,
    gen_line(Size1, List1, Value),
    List = [Value | List1].

%define translations
traducao(0, 32). %space
traducao(1, 9634). %square
traducao(2, 9711). %ball
traducao(3, 9635). %filled square
traducao(4, 9673). %filled ball
traducao(-1, 9472). %horizontal line