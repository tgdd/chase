{
  open PolicyParser
}

let alpha = ['a'-'z''A'-'Z']
let num = ['0'-'9']
let filename = (alpha|num|['.''-''/'])*
let iden = alpha(alpha|num)*
let newline = ['\n''\r']|"\n\r"
let whitespace = [' ' '\t']

rule token = parse
  | whitespace { token lexbuf }
  | newline { LexUtil.incr_lineno lexbuf; token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "Policy" { POLICY }
  | "uses" { USES }
  | "Target" { TARGET }
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
  | "Variables" { VARIABLES }
  | "Variable" { VARIABLE }
  | '\'' { QUOTE }
  | "Rules" { RULES }
  | ":-" { COLONDASH }
  | "RComb" { RCOMB }
  | "Overrides" { OVERRIDES }
  | "FA" { FA }
  | iden as id { IDEN id }
  | filename as name { FILENAME name }
  | eof { EOF }
