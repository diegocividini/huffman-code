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