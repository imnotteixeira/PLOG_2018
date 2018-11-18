gen_line(0, []).

gen_line(Size, List):-
    Size1 is Size - 1,
    gen_line(Size1, List1),
    List = [0 | List1].

gen_matrix(_, 0, []).

gen_matrix(Width, Height, List):-
    Height1 is Height - 1,
    gen_matrix(Width, Height1, List1),
    gen_line(Width, NewLine),
    List = [NewLine | List1].