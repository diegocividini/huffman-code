Carbutti Lucia, Cividini Diego - Huffman Codes

Il progetto Huffman Codes implementa la codifica di Huffman in Common Lisp

Note di implementazione:
Il progetto è stato implementato in Common Lisp, il file huffman-codes.lisp contiene tutte le funzioni necessarie per la codifica e decodifica di un messaggio utilizzando l'algoritmo di Huffman.
Il metodo di encoding accetta una lista (composta anche da sottoliste) di simboli (che per comportamento nativo di Lisp verranno automaticamente convertiti nel loro corrispettivo in maiuscolo (es. 'a' -> 'A')) o di caratteri (es. #\a) e restituisce l'encoding in una lista di bit.
Si noti che una volta scelto il tipo di simboli inseriti nell'albero, bisogna attenersi alla formattazione utilizzata (case sensitive o case insensitive) per evitare che Lisp ritorni un errore.
Il metodo accetta sia una logica case sensitive (in quel caso optare per un messaggio in una lista di caratteri) e sia una logica case insensitive (in quel caso optare per un messaggio in una lista di simboli).

Il metodo di decoding accetta una lista di bit (verificandoc che sia composta da cifre binarie) e un albero di Huffman e restituisce il messaggio sottoforma di lista di caratteri.

L'albero di Huffman deve necessariamente contenere tutti i simboli da codificare/decodificare, altrimenti verrà generato un errore.

Metodi implementati:
Metodi obbligatori da specifica (nome_metodo parametri output): 
hucodec-decode                      bits huffman-tree                     -> message
hucodec-encode                      message huffman-tree                  -> bits
hucodec-encode-file                 filename huffman-tree                 -> bits
hucodec-generate-huffman-tree       symbols-n-weights                     -> huffman-tree
hucodec-generate-symbol-bits-table  huffman-tree                          -> symbol-bits-table
hucodec-print-huffman-tree          huffman-tree &optional (indent 0)     -> NIL

Ulteriori metodi implementati (nome_metodo parametri output):
flatten-list                        lst                                   -> lst
    Il metodo flatten-list prende in input una lista di liste e restituisce una lista con tutti gli elementi delle liste di input
merge-nodes                         queue                                 -> lst
    Il metodo merge-nodes prende in input una lista di nodi e restituisce una lista di nodi ordinata per peso crescente
traverse                            node prefix                           -> lst
    Il metodo traverse prende in input un nodo e un prefisso e restituisce una lista di coppie simbolo-prefisso
validate-bits                       bits                                  -> NIL
    Il metodo validate-bits prende in input una lista di bit e verifica che siano tutti 0 o 1
decode-bits                         bits huffman-tree                     -> message
    Il metodo decode-bits prende in input una lista di bit e un albero di Huffman e restituisce il messaggio decodificato
print-node                          node indent prefix                    -> NIL
    Il metodo print-node stampa un nodo dell'albero di Huffman
collect-symbols                     node                                  -> lst
    Il metodo collect-symbols prende in input un nodo e restituisce una lista di simboli presenti nell'albero di Huffman
file-to-char-list                   stream                                -> lst
    Il metodo file-to-char-list prende in input un file e restituisce una lista di caratteri