% -*- Mode: Prolog -*-
% Carbutti Lucia, Cividini Diego

% huffman-codes.pl starts here

% Helper predicate to count occurrences of a character in a list
% count_occurrences/3 Char List Count
count_occurrences(_, [], 0).
count_occurrences(Char, [Char|Rest], Count) :-
    count_occurrences(Char, Rest, RestCount),
    Count is RestCount + 1.
count_occurrences(Char, [OtherChar|Rest], Count) :-
    Char \= OtherChar,
    count_occurrences(Char, Rest, Count).

% Generate a list of symbols and their weights from a list of characters
% generate_symbols_and_weights/3 Chars Seen SymbolsAndWeights
generate_symbols_and_weights([], _, []).
generate_symbols_and_weights([Char|Chars], Seen,
			     [sw(Char, Count)|SymbolsAndWeights]) :-
    \+ member(Char, Seen),
    count_occurrences(Char, [Char|Chars], Count),
    generate_symbols_and_weights(Chars, [Char|Seen], SymbolsAndWeights).
generate_symbols_and_weights([Char|Chars], Seen, SymbolsAndWeights) :-
    member(Char, Seen),
    generate_symbols_and_weights(Chars, Seen, SymbolsAndWeights).

% Predicate to insert an element into a sorted list
% insert_sorted/3 Element List SortedList
insert_sorted(sw(Char, Count), [], [sw(Char, Count)]).
insert_sorted(sw(Char, Count), [sw(Char1, Count1)|Rest],
	      [sw(Char, Count), sw(Char1, Count1)|Rest]) :-
    Count =< Count1.
insert_sorted(sw(Char, Count), [sw(Char1, Count1)|Rest],
	      [sw(Char1, Count1)|SortedRest]) :-
    Count > Count1,
    insert_sorted(sw(Char, Count), Rest, SortedRest).

% Predicate to perform insertion sort on a list of tuples
% insertion_sort/2 List SortedList
insertion_sort([], []).
insertion_sort([Head|Tail], SortedList) :-
    insertion_sort(Tail, SortedTail),
    insert_sorted(Head, SortedTail, SortedList).

% Main predicate to generate a list of symbols and their weights and sort it
% generate_symbols_and_weights_list/2 Text SortedSymbolsAndWeights
generate_symbols_and_weights_list(Text, SortedSymbolsAndWeights) :-
    string_chars(Text, Chars),
    generate_symbols_and_weights(Chars, [], SymbolsAndWeights),
    insertion_sort(SymbolsAndWeights, SortedSymbolsAndWeights).

% Predicate to create leaf nodes from symbols and weights
% create_leaf_nodes/2 SymbolsAndWeights LeafNodes
create_leaf_nodes([], []).
create_leaf_nodes([sw(Char, Count)|Rest],
		  [node([Char], Count, nil, nil)|LeafNodes]) :-
    create_leaf_nodes(Rest, LeafNodes).

% Predicate to combine two nodes
% combine_nodes/3 Node1 Node2 CombinedNode
combine_nodes(node(Symbols1, Weight1, Left1, Right1),
	      node(Symbols2, Weight2, Left2, Right2),
	      node(Symbols, Weight, node(Symbols1, Weight1, Left1, Right1),
		   node(Symbols2, Weight2, Left2, Right2))) :-
    append(Symbols1, Symbols2, Symbols),
    Weight is Weight1 + Weight2.

% Predicate to get the weight of a node
% node_weight/2 Node Weight
node_weight(node(_, Weight, _, _), Weight).

% Predicate to insert a node into a sorted list of nodes
% insert_node_sorted/3 Node Nodes SortedNodes
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
% build_huffman_tree/2 Nodes HuffmanTree
build_huffman_tree([Node], Node).
build_huffman_tree([Node1, Node2|Rest], HuffmanTree) :-
    combine_nodes(Node1, Node2, CombinedNode),
    insert_node_sorted(CombinedNode, Rest, NewNodes),
    build_huffman_tree(NewNodes, HuffmanTree).

% Main predicate to generate the Huffman tree from list of symbols and weights
% hucodec_generate_huffman_tree/2 SymbolsAndWeights HuffmanTree
hucodec_generate_huffman_tree(SortedSymbolsAndWeights, HuffmanTree) :-
    create_leaf_nodes(SortedSymbolsAndWeights, LeafNodes),
    build_huffman_tree(LeafNodes, HuffmanTree).

% Predicate to traverse the Huffman tree and collect the bit patterns as lists
% traverse_huffman_tree/3 Node Bits SymbolBitsTable
traverse_huffman_tree(node([Symbol], _, nil, nil), Bits, [sb(Symbol, Bits)]).
traverse_huffman_tree(node(_, _, Left, Right), Bits, SymbolBitsTable) :-
    append(Bits, [0], LeftBits),
    append(Bits, [1], RightBits),
    traverse_huffman_tree(Left, LeftBits, LeftTable),
    traverse_huffman_tree(Right, RightBits, RightTable),
    append(LeftTable, RightTable, SymbolBitsTable).

% Predicate to generate the symbol bits table from the Huffman tree
% hucodec_generate_symbol_bits_table/2 HuffmanTree SymbolBitsTable
hucodec_generate_symbol_bits_table(node([Symbol], _, nil, nil),
				   [sb(Symbol, [0])]) :- !.
hucodec_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable) :-
    traverse_huffman_tree(HuffmanTree, [], SymbolBitsTable).

% Predicate to print the Huffman tree in a human-friendly way
% print_huffman_tree/2 HuffmanTree Indent
print_huffman_tree(node(Symbols, Weight, nil, nil), Indent) :-
    format('~wLeaf: ~w, Weight: ~w~n', [Indent, Symbols, Weight]).
print_huffman_tree(node(Symbols, Weight, Left, Right), Indent) :-
    format('~wNode: ~w, Weight: ~w~n', [Indent, Symbols, Weight]),
    atom_concat(Indent, '  ', NewIndent),
    format('~wLeft:~n', [NewIndent]),
    print_huffman_tree(Left, NewIndent),
    format('~wRight:~n', [NewIndent]),
    print_huffman_tree(Right, NewIndent).

% Main predicate to print the Huffman tree
% hucodec_print_huffman_tree/1 HuffmanTree
hucodec_print_huffman_tree(HuffmanTree) :-
    print_huffman_tree(HuffmanTree, '').

% Predicate to find the bits for a given symbol in the symbol bits table
% find_bits/3 Symbol SymbolBitsTable Bits
find_bits(Symbol, [sb(Symbol, Bits)|_], Bits).
find_bits(Symbol, [sb(_, _)|Rest], Bits) :-
    find_bits(Symbol, Rest, Bits).

% Helper predicate to flatten a nested list
% flatten_message/2 NestedList FlatList
flatten_message([], []).
flatten_message([Head|Tail], FlatList) :-
    flatten_message(Head, FlatHead),
    flatten_message(Tail, FlatTail),
    append(FlatHead, FlatTail, FlatList).
flatten_message(Element, [Element]) :-
    \+ is_list(Element).

% Predicate to encode a message using the Huffman tree
% encode_message/3 Chars SymbolBitsTable Bits
encode_message([], _, []).
encode_message([Char|Rest], SymbolBitsTable, Bits) :-
    find_bits(Char, SymbolBitsTable, CharBits),
    encode_message(Rest, SymbolBitsTable, RestBits),
    append(CharBits, RestBits, Bits).

% Main predicate to encode a message using the Huffman tree
% hucodec_encode/3 Message HuffmanTree Bits
hucodec_encode(Message, HuffmanTree, Bits) :-
    flatten_message(Message, FlatMessage),
    hucodec_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable),
    encode_message(FlatMessage, SymbolBitsTable, Bits).

% Predicate to decode bits into a message using the Huffman tree
% decode_bits/4 Bits HuffmanTree CurrentNode Message
decode_bits([], node([Symbol], _, nil, nil), _, [Symbol]).
decode_bits([], _, _, []).
decode_bits([Bit|Bits], node([Symbol], _, nil, nil),
	    HuffmanTree, [Symbol|RestMessage]) :-
    decode_bits([Bit|Bits], HuffmanTree, HuffmanTree, RestMessage).
decode_bits([0|Bits], node(_, _, Left, _), HuffmanTree, Message) :-
    decode_bits(Bits, Left, HuffmanTree, Message).
decode_bits([1|Bits], node(_, _, _, Right), HuffmanTree, Message) :-
    decode_bits(Bits, Right, HuffmanTree, Message).

% Main predicate to decode bits into a message using the Huffman tree
% hucodec_decode/3 Bits HuffmanTree Message
hucodec_decode(Bits, HuffmanTree, Message) :-
    decode_bits(Bits, HuffmanTree, HuffmanTree, FlatMessage),
    unflatten_message(FlatMessage, Message).

% Helper predicate to unflatten a list into a nested list
% unflatten_message/2 FlatList NestedList
unflatten_message([], []).
unflatten_message([Head|Tail], [Head|NestedTail]) :-
    unflatten_message(Tail, NestedTail).

% Predicate to encode the contents of a file using the Huffman tree
% hucodec_encode_file/3 Filename HuffmanTree Bits
hucodec_encode_file(Filename, HuffmanTree, Bits) :-
    open(Filename, read, Stream),
    read_string(Stream, _, Content),
    close(Stream),
    string_chars(Content, Chars),
    hucodec_encode(Chars, HuffmanTree, Bits).

% huffman-codes.pl starts here