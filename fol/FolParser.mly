%{
open FolSyntax
open Lexing

let parse_error s =
  let start_pos = Parsing.symbol_start_pos() in
  let end_pos = Parsing.symbol_end_pos() in
	  print_string s;
    (Printf.printf " at %d:%d-%d:%d\n"
	    start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
	    end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol))

%}

%token LPAREN RPAREN
%token AND OR NOT IMPLIES IFF EXISTS FORALL EQUALS
%token VARIABLES VARIABLE  
%token QUOTE
%token EOF
%token <string> LOWID CAPID

%type <FolSyntax.theory> parse_theory
%start parse_theory

%%

parse_theory: formulas { $1 }

formulas: formulas2 { List.rev $1 }
formulas2: /*nothing*/ { [] }
        | formulas2 formula { $2 :: $1 }
formula: atomic_formula { $1 }
       | LPAREN AND formulas RPAREN { And $3 }
       | LPAREN OR formulas RPAREN { Or $3 }
       | LPAREN NOT formula RPAREN { Not $3 }
       | LPAREN IMPLIES formula formula RPAREN { Implies($3, $4) }
       | LPAREN IFF formula formula RPAREN { Iff($3, $4) }
       | LPAREN EXISTS LOWID CAPID formula RPAREN { Exists($3, $4, $5) }
       | LPAREN FORALL LOWID CAPID formula RPAREN { Forall($3, $4, $5) }
atomic_formula: LPAREN term EQUALS term RPAREN { Equals($2, $4) }
              | LPAREN LOWID terms RPAREN { Pred($2, $3) }
terms: terms2 { List.rev $1 }
terms2: term { [$1] }
      | terms2 term { $2 :: $1 }
term: LOWID { Var $1 }
    | QUOTE LOWID { FunApp($2,[]) }
    | LOWID LPAREN terms RPAREN { FunApp($1, $3) }
