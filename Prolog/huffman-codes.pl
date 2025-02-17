% Carbutti Lucia, Cividini Diego

%% hucodec_decode/3 Bits HuffmanTree Message
%% hucodec_encode/3 Message HuffmanTree Bits
%% hucodec_encode_file/3 Filename HuffmanTree Bits
%% hucodec_generate_huffman_tree/2 SymbolsAndWeights HuffmanTree
%% hucodec_generate_symbol_bits_table/2 HuffmanTree SymbolBitsTable
%% hucodec_print_huffman_tree/1 HuffmanTree

% Check if a character is an ASCII character
is_ascii(Char) :-
    char_code(Char, Code),
    Code >= 0,
    Code =< 127.

% Check if all characters in a list are ASCII characters
all_ascii([]).
all_ascii([Char|Rest]) :-
    is_ascii(Char),
    all_ascii(Rest).

% Check if a text (string) contains only ASCII characters
text_is_ascii(Text) :-
    string_chars(Text, Chars),
    all_ascii(Chars).

% Helper predicate to count occurrences of a character in a list
count_occurrences(_, [], 0).
count_occurrences(Char, [Char|Rest], Count) :-
    count_occurrences(Char, Rest, RestCount),
    Count is RestCount + 1.
count_occurrences(Char, [OtherChar|Rest], Count) :-
    Char \= OtherChar,
    count_occurrences(Char, Rest, Count).

% Generate a list of symbols and their weights from a list of characters
generate_symbols_and_weights([], _, []).
generate_symbols_and_weights([Char|Chars], Seen, [sw(Char, Count)|SymbolsAndWeights]) :-
    \+ member(Char, Seen),
    count_occurrences(Char, [Char|Chars], Count),
    generate_symbols_and_weights(Chars, [Char|Seen], SymbolsAndWeights).
generate_symbols_and_weights([Char|Chars], Seen, SymbolsAndWeights) :-
    member(Char, Seen),
    generate_symbols_and_weights(Chars, Seen, SymbolsAndWeights).

% Predicate to insert an element into a sorted list
insert_sorted(sw(Char, Count), [], [sw(Char, Count)]).
insert_sorted(sw(Char, Count), [sw(Char1, Count1)|Rest], [sw(Char, Count), sw(Char1, Count1)|Rest]) :-
    Count =< Count1.
insert_sorted(sw(Char, Count), [sw(Char1, Count1)|Rest], [sw(Char1, Count1)|SortedRest]) :-
    Count > Count1,
    insert_sorted(sw(Char, Count), Rest, SortedRest).

% Predicate to perform insertion sort on a list of tuples
insertion_sort([], []).
insertion_sort([Head|Tail], SortedList) :-
    insertion_sort(Tail, SortedTail),
    insert_sorted(Head, SortedTail, SortedList).

% Main predicate to generate a list of symbols and their weights from a text and sort it
generate_symbols_and_weights_list(Text, SortedSymbolsAndWeights) :-
    string_chars(Text, Chars),
    generate_symbols_and_weights(Chars, [], SymbolsAndWeights),
    insertion_sort(SymbolsAndWeights, SortedSymbolsAndWeights).

% Predicate to create leaf nodes from symbols and weights
create_leaf_nodes([], []).
create_leaf_nodes([sw(Char, Count)|Rest], [leaf(Char, Count)|LeafNodes]) :-
    create_leaf_nodes(Rest, LeafNodes).

% Predicate to combine two nodes
combine_nodes(Node1, Node2, combined(Node1, Node2, Weight)) :-
    node_weight(Node1, Weight1),
    node_weight(Node2, Weight2),
    Weight is Weight1 + Weight2.

% Predicate to get the weight of a node
node_weight(leaf(_, Weight), Weight).
node_weight(combined(_, _, Weight), Weight).

% Predicate to insert a node into a sorted list of nodes
insert_node_sorted(Node, [], [Node]).
insert_node_sorted(Node, [Node1|Rest], [Node, Node1|Rest]) :-
    node_weight(Node, Weight),
    node_weight(Node1, Weight1),
    Weight =< Weight1.
insert_node_sorted(Node, [Node1|Rest], [Node1|SortedRest]) :-
    node_weight(Node, Weight),
    node_weight(Node1, Weight1),
    Weight > Weight1,
    insert_node_sorted(Node, Rest, SortedRest).

% Predicate to build the Huffman tree from a list of nodes
build_huffman_tree([Node], Node).
build_huffman_tree([Node1, Node2|Rest], HuffmanTree) :-
    combine_nodes(Node1, Node2, CombinedNode),
    insert_node_sorted(CombinedNode, Rest, NewNodes),
    build_huffman_tree(NewNodes, HuffmanTree).

% Generate a Huffman tree from a list of symbols and their weights
hucodec_generate_huffman_tree(Text, HuffmanTree) :-
    generate_symbols_and_weights_list(Text, SortedSymbolsAndWeights),
    create_leaf_nodes(SortedSymbolsAndWeights, LeafNodes),
    build_huffman_tree(LeafNodes, HuffmanTree).