% -------------------------------
% DECODIFICA UNA SEQUENZA DI BIT
% -------------------------------

hucodec_decode(Bits, HuffmanTree, Message):-
    decode(Bits, HuffmanTree, Char, BitsTail),
    hucodec_decode(BitsTail, HuffmanTree, Rest),
    append(Char, Rest, Message).
hucodec_decode([], _, []).

decode([], node(Sym, _, nil, nil), Sym, []).
%Sinistra
decode([0|Bits], node(_, _, Left, _), Msg, RestBits):-
    Left \= nil,
    !,
    decode(Bits, Left, Msg, RestBits).
decode([0|Bits], node(Sym, _, nil, nil), Sym, [0|Bits]):-!.
%Destra
decode([1|Bits], node(_, _, _, Right), Msg, RestBits):-
    Right \= nil,
    !,
    decode(Bits, Right, Msg, RestBits).
%Caso base
decode([1|Bits], node(Sym, _, nil, nil), Sym, [1|Bits]):-!.

% -------------------------------
% CODIFICA UNA SEQUENZA DI SIMBOLI
% -------------------------------

hucodec_encode(Message, HuffmanTree, Bits):-
    is_list(Message),
    hucodec_encode_list(Message, HuffmanTree, Bits).

hucodec_encode_list([MsgSym|MsgTail], HuffmanTree, Bits):-
    encode(MsgSym, HuffmanTree, MsgSymCode),
    hucodec_encode_list(MsgTail, HuffmanTree, Rest),
    append(MsgSymCode, Rest, Bits).
hucodec_encode_list([], _, []).

%Sinistra
encode(MsgSym, node(_, _, node(Sym, W, Left, Right), _), CharCode):-
    member(MsgSym, Sym),
    !,
    encode(MsgSym, node(Sym, W, Left, Right), Rest),
    append([0], Rest, CharCode).
%Destra
encode(MsgSym, node(_, _, _, node(Sym, W, Left, Right)), CharCode):-
    member(MsgSym, Sym),
    !,
    encode(MsgSym, node(Sym, W, Left, Right), Rest),
    append([1], Rest, CharCode).
%Caso base
encode(MsgSym, node([MsgSym], _, nil, nil), []).

% -------------------------------
% LETTURA DI UN FILE E CODIFICA
% -------------------------------

hucodec_encode_file(Filename, HuffmanTree, Bits):-
    open(Filename, read, Str),
    read_stream_to_codes(Str, Codes),
    close(Str),
    codes_to_chars(Codes, List),
    %vedi se rimuovere write
    write(List), nl,
    hucodec_encode(List, HuffmanTree, Bits).

codes_to_chars([], []).
codes_to_chars([C|Cs], [Char|Chars]) :-
    char_code(Char, C),
    codes_to_chars(Cs, Chars).

% -------------------------------
% GENERA UN ALBERO DI HUFFMAN
% -------------------------------

hucodec_generate_huffman_tree(SWs, Tree):-
    inizializza_nodi(SWs, Nodes),
    create_tree(Nodes, Tree).

% Trasforma la lista di coppie (Simbolo, Peso) in una lista di nodi
inizializza_nodi(SWs, Nodes) :-
    findall(node([Sym], W, nil, nil), member(sw(Sym, W), SWs), Nodes).

create_tree([Tree], Tree):- !.
create_tree(Nodes, Tree):-
    selezione_due_minimi(Nodes, Min1, Min2, RestNodes),
    combina_nodi(Min1, Min2, NewNode),
    append(RestNodes, [NewNode], NewNodes),
    create_tree(NewNodes, Tree).

selezione_due_minimi(Nodes, Min1, Min2, RestNodes):-
    list_min(Nodes, Min1),
    rmv_min(Nodes, Min1, NewNodes),
    list_min(NewNodes, Min2),
    rmv_min(NewNodes, Min2, RestNodes).

list_min([node(Sym, W, Left, Right)|Ls], node(SymMin, Min, LeftMin, RightMin)) :-
    list_min(Ls, node(Sym, W, Left, Right), node(SymMin, Min, LeftMin, RightMin)).
list_min([], Min, Min).
list_min([node(Sym1, W, Left1, Right1)|Ls], node(_, Min0, _, _), node(SymMin, Min, LeftMin, RightMin)) :-
    W < Min0,
    !,
    list_min(Ls, node(Sym1, W, Left1, Right1), node(SymMin, Min, LeftMin, RightMin)).
list_min([node(_, W, _, _)|Ls], node(Sym0, Min0, Left0, Right0), node(SymMin, Min, LeftMin, RightMin)) :-
    W >= Min0,
    !,
    list_min(Ls, node(Sym0, Min0, Left0, Right0), node(SymMin, Min, LeftMin, RightMin)).

rmv_min([], _, []).
rmv_min([node(Sym, Min, Left, Right)|More], node(Sym, Min, Left, Right), More):-!.
rmv_min([H|More], node(Sym, Min, Left, Right), [H|NewList]):-
    rmv_min(More, node(Sym, Min, Left, Right), NewList).

combina_nodi(node(S1, W1, L1, R1), node(S2, W2, L2, R2), node(S, W, node(S1, W1, L1, R1),node(S2, W2, L2, R2))) :-
    W is W1 + W2,
    append(S1, S2, S).

% -------------------------------
% GENERA UNA TABELLA SIMBOLO-BIT
% -------------------------------

hucodec_generate_symbol_bits_table(node([], _, _, _), []):- !.
hucodec_generate_symbol_bits_table(node(Sym, W, Left, Right), SymbolBitsTable):-
    generate_symbol_table(node(Sym, W, Left, Right), Sym, SymbolBitsTable).

generate_symbol_table(_, [], []):- !.
generate_symbol_table(HF, [Symbol|Tail], SymbolBitsTable):-
    hucodec_encode([Symbol], HF, Code),
    generate_symbol_table(HF, Tail, Rest),
    append([sb(Symbol, Code)], Rest, SymbolBitsTable).

% -------------------------------
% STAMPA L'ALBERO DI HUFFMAN
% -------------------------------

hucodec_print_huffman_tree(Tree) :- hucodec_print_huffman_tree(Tree, 4).

hucodec_print_huffman_tree(nil, _).
hucodec_print_huffman_tree(node(Sym, W, Left, Right), Spacing) :-
    format("~w (~w)~n", [Sym, W]),
    print_left(Left, Spacing),
    print_right(Right, Spacing).

print_left(nil, _):- !.
print_left(Left, Spacing) :-
    tab(Spacing),
    write("L-> "),
    NextSpacing is Spacing + 4,
    hucodec_print_huffman_tree(Left, NextSpacing).

print_right(nil, _):- !.
print_right(Right, Spacing) :-
    tab(Spacing),
    write("R-> "),
    NextSpacing is Spacing + 4,
    hucodec_print_huffman_tree(Right, NextSpacing).
