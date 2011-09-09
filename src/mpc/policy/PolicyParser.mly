%{
open Batteries
open Printf
open SortedFol.Syntax
open PolicySyntax
open Lexing

let parse_error s =
  print_string s;
  let start_pos = Parsing.symbol_start_pos() in
  let end_pos = Parsing.symbol_end_pos() in
    (printf " at %d:%d-%d:%d\n"
	    start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
	    end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol))

%}

%token LPAREN RPAREN
       POLICY USES
       TARGET
       AND OR NOT IMPLIES IFF EXISTS FORALL EQUALS
       TRUE FALSE
       VARIABLES VARIABLE  
       QUOTE
       RULES COLONDASH
       RCOMB OVERRIDES FA
       EOF
%token <string> IDEN FILENAME

%type <PolicySyntax.policy> parse
%start parse

%%

parse: LPAREN POLICY USES FILENAME 
            target_clause 
            variables_clause 
            rules_clause 
            rcomb_clause  RPAREN EOF
        { { uses = $4; target = $5; vars = $6; rules = $7; dcrds = $8 } }
     | LPAREN POLICY USES FILENAME 
            target_clause 
            variables_clause 
            rules_clause  RPAREN EOF
        { { uses = $4; target = $5; vars = $6; rules = $7
          ; dcrds = [] } }
     | LPAREN POLICY USES FILENAME 
            variables_clause 
            rules_clause 
            rcomb_clause  RPAREN EOF
        { { uses = $4; target = tru; vars = $5; rules = $6
          ; dcrds = $7 } }
     | LPAREN POLICY USES FILENAME 
            variables_clause 
            rules_clause  RPAREN EOF
        { { uses = $4; target = tru; vars = $5; rules = $6
          ; dcrds = []} }

target_clause: LPAREN TARGET formula RPAREN { $3 }
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


variables_clause: LPAREN VARIABLES var_decls RPAREN { $3 }
var_decls: var_decls_rev { List.rev $1 }
var_decls_rev: var_decl { [$1] }
          | var_decls_rev var_decl { $2 :: $1 }
var_decl: LPAREN VARIABLE IDEN IDEN RPAREN { { variable_name = $3; variable_sort = $4 } }

rules_clause: LPAREN RULES rules RPAREN { $3 }
rules: rules_rev { List.rev $1 }
rules_rev: rule { [$1] }
      | rules_rev rule { $2 :: $1 }
rule: LPAREN IDEN EQUALS LPAREN IDEN var_list RPAREN COLONDASH formulas RPAREN
      { { rule_name = $2; rule_decision = $5; rule_parameters = $6
        ; rule_body = if not (List.is_empty $9) && List.is_empty (List.tl $9)
                      then List.hd $9 else And($9) } }

var_list: var_list_rev { List.rev $1 }
var_list_rev: IDEN { [$1] }
            | var_list_rev IDEN { $2 :: $1 }

rcomb_clause: LPAREN RCOMB rcombs RPAREN { $3 }
rcombs: rcombs_rev { List.rev $1 }

rcombs_rev: /*nothing*/ { [] }
          | rcombs_rev LPAREN rcomb RPAREN { $3 :: $1 }

rcomb: FA decisions { FirstApplicable $2 }
     | OVERRIDES IDEN decisions { Overrides ($2, $3) }

decisions: decisions_rev { List.rev $1 }
decisions_rev: /*nothing*/ { [] }
         | decisions_rev IDEN { $2 :: $1 }
