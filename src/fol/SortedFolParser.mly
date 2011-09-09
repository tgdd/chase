%{
open SortedFolFormulas
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
%token TRUE FALSE
%token VARIABLES VARIABLE  
%token QUOTE
%token EOF
%token <string> IDEN

%type <SortedFolFormulas.theory> parse_theory
%start parse_theory
%type <SortedFolFormulas.formula> parse_formula
%start parse_formula

%%

parse_theory: formulas { $1 }
parse_formula: formula { $1 }

formulas: formulas_rev { List.rev $1 }
formulas_rev: /*nothing*/ { [] }
            | formulas_rev formula { $2 :: $1 }
formula: atomic_formula { $1 }
       | LPAREN AND formulas RPAREN { And $3 }
       | LPAREN OR formulas RPAREN { Or $3 }
       | LPAREN NOT formula RPAREN { Not $3 }
       | LPAREN IMPLIES formula formula RPAREN { Implies($3, $4) }
       | LPAREN IFF formula formula RPAREN { Iff($3, $4) }
       | LPAREN EXISTS IDEN IDEN formula RPAREN { Exists($3, $4, $5) }
       | LPAREN FORALL IDEN IDEN formula RPAREN { Forall($3, $4, $5) }
       | TRUE { tru }
       | FALSE { fals }
atomic_formula: LPAREN term EQUALS term RPAREN { Equals($2, $4) }
              | LPAREN IDEN terms RPAREN { Atom($2, $3) }

terms: terms_rev { List.rev $1 }
terms_rev: term { [$1] }
         | terms_rev term { $2 :: $1 }
term: IDEN { Var $1 }
    | QUOTE IDEN { FunApp($2,[]) }
    | IDEN LPAREN terms RPAREN { FunApp($1, $3) }
