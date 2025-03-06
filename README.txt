Huffman Encoding in Prolog

Descrizione:
Questo progetto implementa l'algoritmo di codifica di Huffman utilizzando Prolog. La codifica di Huffman è un metodo di compressione senza perdita di dati che consente di rappresentare simboli frequenti con codici brevi e simboli rari con codici più lunghi, ottimizzando così lo spazio di archiviazione.

L'implementazione fornisce i predicati principali per la codifica e la decodifica di messaggi, la generazione dell'albero di Huffman e l'encoding/decoding di file. L'algoritmo è strutturato attraverso l'uso di nodi e funzioni ricorsive per costruire e navigare nell'albero di Huffman.

Predicati Implementati:

1. `hucodec_decode/3`
   - Decodifica una sequenza di bit usando un albero di Huffman.
   - Sintassi: `hucodec_decode(Bits, HuffmanTree, Message).`

2. `decode/4`
   - Decodifica un singolo carattere da una sequenza di bit.
   - Sintassi: `decode(Bits, Node, Symbol, RestBits).`

3. `hucodec_encode/3`
   - Codifica un messaggio usando un albero di Huffman.
   - Sintassi: `hucodec_encode(Message, HuffmanTree, Bits).`

4. `encode/3`
   - Codifica un singolo simbolo secondo l'albero di Huffman.
   - Sintassi: `encode(Symbol, HuffmanTree, Code).`

5. `hucodec_encode_file/3`
   - Codifica il contenuto di un file un carattere alla volta utilizzando un albero di Huffman.
   - Sintassi: `hucodec_encode_file(Filename, HuffmanTree, Bits).`

6. `codes_to_chars/2`
   - Converte una lista di codici ASCII in caratteri.
   - Sintassi: `codes_to_chars(Codes, Chars).`

7. `hucodec_generate_huffman_tree/2`
   - Genera un albero di Huffman da una lista di coppie simbolo-peso.
   - Sintassi: `hucodec_generate_huffman_tree(SymbolWeights, Tree).`

8. `create_tree/2`
   - Costruisce ricorsivamente l'albero di Huffman.
   - Sintassi: `create_tree(Nodes, Tree).`

9. `selezione_due_minimi/4`
   - Seleziona i due nodi con peso minimo dalla lista.
   - Sintassi: `selezione_due_minimi(Nodes, Min1, Min2, RestNodes).`

10. `combina_nodi/3`
   - Combina due nodi in un nuovo nodo dell'albero di Huffman.
   - Sintassi: `combina_nodi(Node1, Node2, NewNode).`

11. `hucodec_generate_symbol_bits_table/2`
   - Genera una tabella simbolo-bit da un albero di Huffman.
   - Sintassi: `hucodec_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable).`

12. `generate_symbol_table/3`
   - Costruisce la tabella dei simboli con i relativi codici.
   - Sintassi: `generate_symbol_table(HuffmanTree, Symbols, SymbolBitsTable).`

13. `hucodec_print_huffman_tree/1`
   - Stampa la struttura dell'albero di Huffman.
   - Sintassi: `hucodec_print_huffman_tree(Tree).`

14. `print_left/2` e `print_right/2`
   - Funzioni di supporto per la stampa dell'albero.
   - Sintassi: `print_left(Left, Spacing).`
