%{
open Printf
open VocabSyntax
open Lexing
open ListUtil

let parse_error s =
  let start_pos = Parsing.symbol_start_pos() in
  let end_pos = Parsing.symbol_end_pos() in
	  print_string s;
    (printf " at %d:%d-%d:%d\n"
	    start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
	    end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol))

%}

%token LPAREN RPAREN
       VOCAB
       TYPES TYPE
       PREDICATES PREDICATE
       CONSTANTS CONSTANT
       FUNCTIONS FUNCTION 
       RANGLE
       EOF
%token <string> LOWID CAPID

%type <VocabSyntax.vocab> start
%start start

%%

start: LPAREN VOCAB id types_clause clauses RPAREN EOF
       { {$5 with vocab_name = $3; sorts = $4} }


id: LOWID { $1 }
  | CAPID { $1 }

/* all 8 cases */
clauses: preds_clause consts_clause funcs_clause
         { { vocab_name = ""; sorts = []; preds = $1; consts = $2; funcs = $3 } }
       | preds_clause consts_clause
         { { vocab_name = ""; sorts = []; preds = $1; consts = $2; funcs = [] } }
       | preds_clause funcs_clause
         { { vocab_name = ""; sorts = []; preds = $1; consts = []; funcs = $2 } }
       | consts_clause funcs_clause
         { { vocab_name = ""; sorts = []; preds = []; consts = $1; funcs = $2 } }
       | funcs_clause
         { { vocab_name = ""; sorts = []; preds = []; consts = []; funcs = $1 } }
       | consts_clause
         { { vocab_name = ""; sorts = []; preds = []; consts = $1; funcs = [] } }
       | preds_clause
         { { vocab_name = ""; sorts = []; preds = $1; consts = []; funcs = [] } }
       | /* nothing */
         { { vocab_name = ""; sorts = []; preds = []; consts = []; funcs = [] } }
       

types_clause: LPAREN TYPES type_decls RPAREN { List.rev $3 }
type_decls: type_decl { [$1] }
          | type_decls type_decl { $2::$1 }
type_decl: LPAREN TYPE CAPID RPAREN { { sort_name = $3; subsorts = [] } }
         | LPAREN TYPE CAPID RANGLE types RPAREN { { sort_name = $3; subsorts = $5 } }
types: types2 { List.rev $1 }
types2: CAPID { [$1] }
     | types2 CAPID { $2::$1 }

preds_clause: LPAREN PREDICATES preds RPAREN { List.rev $3 }
preds: /*nothing*/ { [] }
     | preds pred { $2 :: $1 }
pred: LPAREN PREDICATE LOWID types RPAREN { { pred_name = $3; pred_arity = $4 } }

consts_clause: LPAREN CONSTANTS consts RPAREN { List.rev $3 }
consts: /*nothing*/ { [] }
      | consts const { $2::$1 }
const: LPAREN CONSTANT LOWID CAPID RPAREN { { const_name = $3; const_sort = $4 } }

funcs_clause: LPAREN FUNCTIONS funcs RPAREN { List.rev $3 }
funcs: /*nothing*/ { [] }
     | funcs func { $2::$1 }
func: LPAREN FUNCTION LOWID types RPAREN
      { { func_name = $3; func_arity = initial $4; func_sort = last $4 } }

%%
