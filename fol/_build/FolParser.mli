type token =
  | LPAREN
  | RPAREN
  | AND
  | OR
  | NOT
  | IMPLIES
  | IFF
  | EXISTS
  | FORALL
  | EQUALS
  | VARIABLES
  | VARIABLE
  | QUOTE
  | EOF
  | LOWID of (string)
  | CAPID of (string)

val parse_theory :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> FolSyntax.theory
