%{
open Printf
open SortedFol.Syntax
open PolicySyntax
open Lexing

let parse_error s =
  let start_pos = Parsing.symbol_start_pos() in
  let end_pos = Parsing.symbol_end_pos() in
	  print_string s;
    (printf " at %d:%d-%d:%d\n"
	    start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
	    end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol))

%}

%token LPAREN RPAREN
%token POLICY USES
%token TARGET
%token AND OR NOT IMPLIES IFF EXISTS FORALL EQUALS
%token TRUE FALSE
%token VARIABLES VARIABLE  
%token QUOTE
%token RULES COLONDASH
%token RCOMB OVERRIDES FA
%token EOF
%token <string> LOWID CAPID FULLID

%type <PolicySyntax.policy> start
%start start

%%

start: LPAREN POLICY USES filename 
            target_clause 
            variables_clause 
            rules_clause 
            rcomb_clause  RPAREN EOF
        { { uses = $4; target = $5; vars = $6; rules = $7; rule_combs = $8 } }
     | LPAREN POLICY USES filename 
            target_clause 
            variables_clause 
            rules_clause  RPAREN EOF
        { { uses = $4; target = $5; vars = $6; rules = $7
          ; rule_combs = {rule_fas = [] ; rule_overrides = [] } } }
     | LPAREN POLICY USES filename 
            variables_clause 
            rules_clause 
            rcomb_clause  RPAREN EOF
        { { uses = $4; target = tru; vars = $5; rules = $6
          ; rule_combs = $7 } }
     | LPAREN POLICY USES filename 
            variables_clause 
            rules_clause  RPAREN EOF
        { { uses = $4; target = tru; vars = $5; rules = $6
          ; rule_combs = {rule_fas = [] ; rule_overrides = [] } } }

filename: id { $1 }
        | FULLID { $1 }

id: LOWID { $1 }
  | CAPID { $1 }

target_clause: LPAREN TARGET formula RPAREN { $3 }
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
       | TRUE { tru }
       | FALSE { fals }
atomic_formula: LPAREN term EQUALS term RPAREN { Equals($2, $4) }
              | LPAREN LOWID terms RPAREN { Pred($2, $3) }
terms: terms2 { List.rev $1 }
terms2: term { [$1] }
      | terms2 term { $2 :: $1 }
term: LOWID { Var $1 }
    | QUOTE LOWID { FunApp($2,[]) }
    | LOWID LPAREN terms RPAREN { FunApp($1, $3) }


variables_clause: LPAREN VARIABLES var_decls RPAREN { $3 }
var_decls: var_decls2 { List.rev $1 }
var_decls2: var_decl { [$1] }
          | var_decls2 var_decl { $2 :: $1 }
var_decl: LPAREN VARIABLE LOWID CAPID RPAREN { { variable_name = $3; variable_sort = $4 } }

rules_clause: LPAREN RULES rules RPAREN { $3 }
rules: rules2 { List.rev $1 }
rules2: rule { [$1] }
      | rules2 rule { $2 :: $1 }
rule: LPAREN id EQUALS LPAREN LOWID var_list RPAREN COLONDASH formulas RPAREN
      { { rule_name = $2; rule_decision = $5; rule_parameters = $6; rule_body = And($9) } }

var_list: var_list2 { List.rev $1 }
var_list2: LOWID { [$1] }
         | var_list2 LOWID { $2 :: $1 }

rcomb_clause: LPAREN RCOMB rcombs RPAREN { $3 }
rcombs: rcombs2 { { rule_fas = List.rev $1.rule_fas
                  ; rule_overrides = List.rev $1.rule_overrides } }

rcombs2: /*nothign*/ { { rule_fas = []; rule_overrides = [] } }
       | rcombs2 LPAREN rcomb RPAREN { $3 $1 }

rcomb: FA decisions { fun rc -> { rc with rule_fas = $2::rc.rule_fas } }
     | OVERRIDES LOWID decisions
      { fun rc -> { rc with rule_overrides =
                              { low_rule = $2
                              ; high_rules = $3 } :: rc.rule_overrides }  }

decisions: decisions2 { List.rev $1 }
decisions2: /*nothing*/ { [] }
         | decisions2 LOWID { $2 :: $1 }
