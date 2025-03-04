hucodec_generate_huffman_tree(SymbolsAndWeights, HuffmanTree)
Predicato che data una lista di termini sw(simbolo, peso) costruisce un albero di Huffman
Prima crea l’insieme dei nodi a partire dalla lista con il predicato inizializza_nodi(SymbolsAndWeights, Nodes).
Ogni nodo viene inizializzato con peso e simbolo e senza figli: node(Sym, W, nil, nil).