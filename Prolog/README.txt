Carbutti Lucia, Cividini Diego - Huffman Codes

Il progetto Huffman Codes implementa la codifica di Huffman in Prolog

Note di implementazione:
Il progetto è stato implementato in Prolog, il file huffman_codes.pl contiene tutte le funzioni necessarie per la codifica e decodifica di un messaggio utilizzando l'algoritmo di Huffman.
Il metodo di encoding accetta una lista (composta anche da sottoliste) di caratteri e restituisce l'encoding in una lista di bit.

Il metodo di decoding accetta una lista di bit e un albero di Huffman e restituisce il messaggio sottoforma di lista di caratteri.

L'albero di Huffman deve necessariamente contenere tutti i simboli da codificare/decodificare, altrimenti il predicato restituirà false.

Metodi implementati:
Metodi obbligatori da specifica (nome_metodo parametri):
hucodec_decode/3                          Bits, HuffmanTree, Message
hucodec_encode/3                          Message, HuffmanTree, Bits
hucodec_encode_file/3                     Filename, HuffmanTree, Bits
hucodec_generate_huffman_tree/2           SymbolsAndWeights, HuffmanTree 
hucodec_generate_symbol_bits_table/2      HuffmanTree, SymbolBitsTable
hucodec_print_huffman_tree/1              HuffmanTree

Ulteriori metodi implementati (nome_metodo parametri):
count_occurrences/3                       Char, List, Count
    Conta le occorrenze di un carattere in una lista
generate_symbols_and_weights/3            Chars, Seen, SymbolsAndWeights
    Genera una lista di simboli e i loro pesi da una lista di caratteri.
insert_sorted/3                           Element, List, SortedList
    Inserisce un elemento in una lista ordinata.
insertion_sort/2                          List, SortedList
    Ordina una lista di tuple usando insertion sort
create_leaf_nodes/2                       SymbolsAndWeights, LeafNodes
    Crea nodi foglia dai simboli e pesi
combine_nodes/3                           Node1, Node2, CombinedNode
    Combina due nodi in un nuovo nodo
node_weight/2                             Node, Weight
    Restituisce il peso di un nodo
insert_node_sorted/3                      Node, Nodes, SortedNodes
    Inserisce un nodo in una lista ordinata di nodi
find_min_node/3                           Nodes, MinNode, RestNodes
    Trova il nodo con il peso più piccolo
build_huffman_tree/2                      Nodes, HuffmanTree
    Costruisce l'albero di Huffman da una lista di nodi
traverse_huffman_tree/3                   Node, Bits, SymbolBitsTable
    Percorre l'albero di Huffman e raccoglie i codici binari
print_huffman_tree/2                      (HuffmanTree, Indent)
    Stampa l'albero di Huffman in modo leggibile
find_bits/3                               Symbol, SymbolBitsTable, Bits
    Trova i bit corrispondenti a un simbolo nella tabella dei codici
flatten_message/2                         NestedList, FlatList
    Appiattisce una lista annidata
encode_message/3                          Chars, SymbolBitsTable, Bits
    Codifica un messaggio utilizzando l'albero di Huffman
decode_bits/4                             Bits, HuffmanTree, CurrentNode, Message
    Decodifica una sequenza di bit in un messaggio
unflatten_message/2                       FlatList, NestedList
    Trasforma una lista piatta in una lista annidata
