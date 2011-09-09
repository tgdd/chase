{
  open SortedFolParser 
}

let alpha = ['a'-'z''A'-'Z']
let num = ['0'-'9']
let iden = alpha(alpha|num|"_"|"{"|"}")*
let newline = ['\n''\r']|"\n\r"
let whitespace = [' ' '\t']

rule token = parse
  | whitespace { token lexbuf }
  | newline { LexUtil.incr_lineno lexbuf; token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | "->" { IMPLIES }
  | "iff" { IFF }
  | "exists" { EXISTS }
  | "forall" { FORALL }
  | "true" { TRUE }
  | "false" { FALSE }
  | "=" { EQUALS }
  | '\'' { QUOTE }
  | iden as id { IDEN id }
  | eof { EOF }
