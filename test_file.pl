show_read_characters:-
    write('Insere as coordenadas da tua proxima jogada <0-10,A-K>:'),
    get_char(X),
    get_code(Y),
    get_code(_),
    write('O X e '),
    write(X),
    write(' e o Y e '),
    write(Y).