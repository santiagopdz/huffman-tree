%Progetto

%Coppie sw

%Se simbolo è una stringa converto in atomo
% Questa implementazione funziona ma non so se sia utile perché non
% accetta stringhe in message(?)

%sw(simbolo, peso):-
 %   string(simbolo),
  %  atom_string(simbolo, Atomo),
   % sw(Atomo, peso).

sw(simbolo, peso).

%SymbolsAndWeights è una lista di coppie sw(Symbol, Weight)

%symbols_and_weights(

%hucodec_generate_huffman_tree(SymbolsAndWeights, HuffmanTree)
% Genera un albero di Huffman a partire da una lista di coppie (simbolo,peso)

hucodec_generate_huffman_tree(SWs, Tree):-
    inizializza_nodi(SWs, Nodes),
    create_tree(Nodes, Tree).

% Stampa l'albero binario con indentazione chiara
hucodec_print_huffman_tree(Tree) :- hucodec_print_huffman_tree(Tree, 4).

% Caso base: se il nodo è nil, non stampa nulla
hucodec_print_huffman_tree(nil, _).

% Stampa il nodo corrente e poi i figli sinistro e destro con indentazione
hucodec_print_huffman_tree(node(Sym, W, Left, Right), Spacing) :-
    format("~w (~w)~n", [Sym, W]), % Stampa il nodo

    % Stampa il figlio sinistro (se esiste)
    print_left(Left, Spacing),

    % Stampa il figlio destro (se esiste)
    print_right(Right, Spacing).

% Stampa il figlio sinistro con indentazione maggiore
print_left(nil, _):- !.  % Se il figlio sinistro è nil, non stampa nulla
print_left(Left, Spacing) :-
    tab(Spacing),
    write("L-> "),
    NextSpacing is Spacing + 4,
    hucodec_print_huffman_tree(Left, NextSpacing).

% Stampa il figlio destro con indentazione maggiore
print_right(nil, _):- !. % Se il figlio destro è nil, non stampa nulla
print_right(Right, Spacing) :-
    tab(Spacing),
    write("R-> "),
    NextSpacing is Spacing + 4,
    hucodec_print_huffman_tree(Right, NextSpacing).


%True se Message è la codifica di Bits econdo HuffmanTree

%hucodec_encode/3 Message HuffmanTree Bits

%messaggi forma lista
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



% True se il contenuto del file Filename è la codifica di Bits econdo
% HuffmanTree

%hucodec_encode_file/3 Filename HuffmanTree Bits

hucodec_encode_file(Filename, HuffmanTree, Bits):-
    open(Filename, read, Str),
%    read_file(Str,Lines),
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


%True se la decodifica di Bits secondo HuffmanTree è Message

%hucodec_decode/3 Bits HuffmanTree Message

hucodec_decode(Bits, HuffmanTree, Message):-
    decode(Bits, HuffmanTree, Char, BitsTail),
    hucodec_decode(BitsTail, HuffmanTree, Rest),
    append(Char, Rest, Message).
%    atomics_to_string(ListMsg, Message).

hucodec_decode([], _, []).

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

decode([1|Bits], node(Sym, _, nil, nil), Sym, [1|Bits]):-!.

decode([], node(Sym, _, nil, nil), Sym, []).

%Genera una lista di coppie sb(simbolo, bits)

%Coppie

sb(simbolo, bits).

%hucodec_generate_symbol_bits_table/2 HuffmanTree SymbolBitsTable

hucodec_generate_symbol_bits_table(node([], _, _, _), []):- !.

hucodec_generate_symbol_bits_table(node(Sym, W, Left, Right), SymbolBitsTable):-
    generate_symbol_table(node(Sym, W, Left, Right), Sym, SymbolBitsTable).

generate_symbol_table(_, [], []):- !.

generate_symbol_table(HF, [Symbol|Tail], SymbolBitsTable):-
    hucodec_encode([Symbol], HF, Code),
    generate_symbol_table(HF, Tail, Rest),
    append([sb(Symbol, Code)], Rest, SymbolBitsTable).

%Private predicates-----------------------------------------------------

%inizializza_nodi(SWs, Nodes)
% Trasforma la lista di coppie (Simbolo, Peso) in una lista di nodi
inizializza_nodi(SWs, Nodes) :-
    findall(node([Sym], W, nil, nil), member(sw(Sym, W), SWs), Nodes).

%create_tree(Nodes, Tree)
%Costruisce l'albero ricorsivamente a partire da un insieme di nodi

% Passo Ricorsivo: seleziona i due nodi con peso minimo, li combina in
% un nuovo nodo. Rimuove i due nodi dall'insieme e aggiunge il nuovo
% nodo.

%Caso base: radice dell'albero
create_tree([Tree], Tree):- !.

create_tree(Nodes, Tree):-
    selezione_due_minimi(Nodes, Min1, Min2, RestNodes),
    combina_nodi(Min1, Min2, NewNode),
    append(RestNodes, [NewNode], NewNodes),
    create_tree(NewNodes, Tree).

%selezione_due_minimi(Nodes, Min1, Min2, RestNodes),
%Individua due fra i nodi con peso minimo in un insieme di nodi

%selezione_due_minimi([Nodo1|[Nodo2|Rest]], Min1, Min2, RestNodes):-

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


%combina_nodi(Min1, Min2, NewNode)
%Costruisce il nuovo nodo

combina_nodi(node(S1, W1, L1, R1), node(S2, W2, L2, R2), node(S, W, node(S1, W1, L1, R1),node(S2, W2, L2, R2))) :-
    W is W1 + W2,
    append(S1, S2, S).

%etichetta_new_node (3 versioini diverse in base agli input)
%Dovrebbe essere inutile a questo punto

% etichetta_new_node()
%prende in input due atomi e restituisce una stringa

etichetta_new_node(A, B, [A, B]) :-
    atom(A), atom(B).

%etichetta_new_node()
%prende in input un atomo e una stringa e restituisce una stringa

etichetta_new_node(A, B, [A|B]) :-
    atom(A), is_list(B).

etichetta_new_node(A, B, [B|A]) :-
    is_list(A), atom(B).



%etichetta_new_node()
%prende in input due stringhe e restituuisce la concatenazione

etichetta_new_node(A, B, Risultato) :-
    is_list(A), is_list(B), append(A, B, Risultato).
