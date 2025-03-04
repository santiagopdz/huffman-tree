servono i predicati sw(simbolo, peso), sb(simbolo, bits) e symbols_n_weights??

Input test:

assert(symbols_n_weights([sw(a, 8), sw(12, 3), sw(sw(c, 23), 1), sw([d, d], 1), sw("eee", 1), sw('FF', 1), sw(g, 1), sw(h, 1)])).

assert(message([a,  12,  sw(c, 23),  [d, d], "eee", 'FF'])).

symbols_n_weights(SWs), 
message(M), 
hucodec_generate_huffman_tree(SWs, HT), 
hucodec_encode(M, HT, Bits),
hucodec_decode(Bits, HT, M),
hucodec_print_huffman_tree(HT).
