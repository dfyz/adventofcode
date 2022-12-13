%token <int> INT
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token COMMA
%start parse_packet
%type <Data_13.packet> parse_packet
%%

parse_packet:
    INT { IntP $1 }
    | LEFT_BRACKET parse_list RIGHT_BRACKET { ListP $2 }
    ;

parse_list:
    parse_packet COMMA parse_list { $1 :: $3 }
    | parse_packet { [$1] }
    | { [] }
    ;
