{
open Parser_13
}

rule lex_packet = parse
    | ',' { COMMA }
    | '[' { LEFT_BRACKET }
    | ']' { RIGHT_BRACKET }
    | ['0'-'9']+ as v { INT (int_of_string v) }