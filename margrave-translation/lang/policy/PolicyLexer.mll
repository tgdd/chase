{
  open PolicyParser
}

let alpha = ['a'-'z''A'-'Z']
let num = ['0'-'9']
let lowid = ['a'-'z'](alpha|num)*
let capid = ['A'-'Z'](alpha|num)*
let fullid = ['a'-'z''A'-'Z''0'-'9''/''.''-']*
let id = alpha(alpha|num)*
let newline = ['\n''\r']|"\n\r"
let whitespace = [' ' '\t']

rule token = parse
  | '"' { inquotes lexbuf }
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
  | "=" { EQUALS }
  | "Variables" { VARIABLES }
  | "Variable" { VARIABLE }
  | '\'' { QUOTE }
  | "Rules" { RULES }
  | ":-" { COLONDASH }
  | "RComb" { RCOMB }
  | "Overrides" { OVERRIDES }
  | "FA" { FA }
  | lowid as id { LOWID id }
  | capid as id { CAPID id }
  | fullid as id { FULLID id }
  | eof { EOF }
and inquotes = parse
  | [^'"']* as id { FULLID id  }
  | '"' { token lexbuf }
