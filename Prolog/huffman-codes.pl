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