{
  open VocabParser
  open Lexing
  exception BadToken of string * position * position
  
  let make_error lexbuf =
    BadToken(lexeme lexbuf, lexeme_start_p lexbuf, lexeme_end_p lexbuf) 
}

let alpha = ['a'-'z''A'-'Z']
let num = ['0'-'9']
let iden = alpha(alpha|num)*
let newline = ['\n''\r']|"\n\r"
let whitespace = [' ' '\t'] 

rule token = parse
  | whitespace { token lexbuf }
  | newline { LexUtil.incr_lineno lexbuf; token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "Vocab" { VOCAB }
  | "Types" { TYPES }
  | "Type" { TYPE }
  | "Predicates" { PREDICATES }
  | "Predicate" { PREDICATE }
  | "Constants" { CONSTANTS }
  | "Constant" { CONSTANT }
  | "Functions" { FUNCTIONS }
  | "Function" { FUNCTION }
  | '>' { RANGLE }
  | iden as id { IDEN id }
  | eof { EOF }
  | _ { raise (make_error lexbuf) }