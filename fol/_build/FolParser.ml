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

open Parsing;;
# 2 "FolParser.mly"
open FolSyntax
open Lexing

let parse_error s =
  let start_pos = Parsing.symbol_start_pos() in
  let end_pos = Parsing.symbol_end_pos() in
	  print_string s;
    (Printf.printf " at %d:%d-%d:%d\n"
	    start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
	    end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol))

# 33 "FolParser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* AND *);
  260 (* OR *);
  261 (* NOT *);
  262 (* IMPLIES *);
  263 (* IFF *);
  264 (* EXISTS *);
  265 (* FORALL *);
  266 (* EQUALS *);
  267 (* VARIABLES *);
  268 (* VARIABLE *);
  269 (* QUOTE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  270 (* LOWID *);
  271 (* CAPID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\005\000\005\000\007\000\008\000\
\008\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\001\000\001\000\000\000\002\000\001\000\004\000\004\000\004\000\
\005\000\005\000\006\000\006\000\005\000\004\000\001\000\001\000\
\002\000\001\000\002\000\004\000\002\000"

let yydefred = "\000\000\
\003\000\000\000\021\000\001\000\000\000\000\000\004\000\005\000\
\003\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\000\000\000\000\016\000\000\000\000\000\000\000\
\006\000\007\000\008\000\000\000\000\000\000\000\000\000\000\000\
\014\000\017\000\000\000\009\000\010\000\000\000\000\000\020\000\
\013\000\011\000\012\000"

let yydgoto = "\002\000\
\003\000\004\000\005\000\007\000\008\000\029\000\030\000\031\000"

let yysindex = "\008\000\
\000\000\000\000\000\000\000\000\009\255\028\255\000\000\000\000\
\000\000\000\000\009\255\009\255\009\255\001\255\002\255\007\255\
\004\255\012\255\021\255\022\255\025\255\009\255\009\255\013\255\
\023\255\000\000\249\254\038\255\000\000\041\255\249\254\249\254\
\000\000\000\000\000\000\042\255\043\255\009\255\009\255\044\255\
\000\000\000\000\045\255\000\000\000\000\046\255\047\255\000\000\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\255\000\000\000\000\048\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\002\000\000\000\247\255\000\000\250\255\024\000\000\000"

let yytablesize = 259
let yytable = "\018\000\
\002\000\021\000\022\000\023\000\027\000\016\000\028\000\018\000\
\001\000\006\000\019\000\020\000\036\000\037\000\024\000\025\000\
\016\000\028\000\018\000\018\000\026\000\032\000\033\000\034\000\
\042\000\043\000\035\000\038\000\046\000\047\000\009\000\010\000\
\011\000\012\000\013\000\014\000\015\000\039\000\027\000\018\000\
\016\000\017\000\041\000\044\000\045\000\048\000\049\000\050\000\
\051\000\015\000\040\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000"

let yycheck = "\006\000\
\000\000\011\000\012\000\013\000\001\001\013\001\014\001\002\001\
\001\000\001\001\009\000\010\000\022\000\023\000\014\001\014\001\
\013\001\014\001\013\001\014\001\014\001\010\001\002\001\002\001\
\031\000\032\000\002\001\015\001\038\000\039\000\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\015\001\001\001\010\001\
\013\001\014\001\002\001\002\001\002\001\002\001\002\001\002\001\
\002\001\002\001\027\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  AND\000\
  OR\000\
  NOT\000\
  IMPLIES\000\
  IFF\000\
  EXISTS\000\
  FORALL\000\
  EQUALS\000\
  VARIABLES\000\
  VARIABLE\000\
  QUOTE\000\
  EOF\000\
  "

let yynames_block = "\
  LOWID\000\
  CAPID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formulas) in
    Obj.repr(
# 27 "FolParser.mly"
                       ( _1 )
# 199 "FolParser.ml"
               : FolSyntax.theory))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formulas2) in
    Obj.repr(
# 29 "FolParser.mly"
                    ( List.rev _1 )
# 206 "FolParser.ml"
               : 'formulas))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "FolParser.mly"
                       ( [] )
# 212 "FolParser.ml"
               : 'formulas2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formulas2) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 31 "FolParser.mly"
                            ( _2 :: _1 )
# 220 "FolParser.ml"
               : 'formulas2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_formula) in
    Obj.repr(
# 32 "FolParser.mly"
                        ( _1 )
# 227 "FolParser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'formulas) in
    Obj.repr(
# 33 "FolParser.mly"
                                    ( And _3 )
# 234 "FolParser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'formulas) in
    Obj.repr(
# 34 "FolParser.mly"
                                   ( Or _3 )
# 241 "FolParser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 35 "FolParser.mly"
                                   ( Not _3 )
# 248 "FolParser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 36 "FolParser.mly"
                                               ( Implies(_3, _4) )
# 256 "FolParser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 37 "FolParser.mly"
                                           ( Iff(_3, _4) )
# 264 "FolParser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 38 "FolParser.mly"
                                                  ( Exists(_3, _4, _5) )
# 273 "FolParser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 39 "FolParser.mly"
                                                  ( Forall(_3, _4, _5) )
# 282 "FolParser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 40 "FolParser.mly"
                                               ( Equals(_2, _4) )
# 290 "FolParser.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'terms) in
    Obj.repr(
# 41 "FolParser.mly"
                                          ( Pred(_2, _3) )
# 298 "FolParser.ml"
               : 'atomic_formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'terms2) in
    Obj.repr(
# 42 "FolParser.mly"
              ( List.rev _1 )
# 305 "FolParser.ml"
               : 'terms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 43 "FolParser.mly"
             ( [_1] )
# 312 "FolParser.ml"
               : 'terms2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'terms2) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 44 "FolParser.mly"
                    ( _2 :: _1 )
# 320 "FolParser.ml"
               : 'terms2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "FolParser.mly"
            ( Var _1 )
# 327 "FolParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "FolParser.mly"
                  ( FunApp(_2,[]) )
# 334 "FolParser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'terms) in
    Obj.repr(
# 47 "FolParser.mly"
                                ( FunApp(_1, _3) )
# 342 "FolParser.ml"
               : 'term))
(* Entry parse_theory *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parse_theory (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : FolSyntax.theory)
