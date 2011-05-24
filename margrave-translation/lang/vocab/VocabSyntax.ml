open StringUtil

(* (Type A > B C) (Type D) mean type A is a supertype of B and C, D is a type *)
type type_decl = { type_name : string
                 ; subtypes : string list
                 }

(* (Pred R A B C) means predicate R is of type AxBxC *)
type pred = { pred_name : string
            ; pred_type : string list
            }

(* (Const x A) means x is a constant of type A *)
type const = { const_name : string
             ; const_type : string
             }

(* (Function f A B C) means f is a function from AxB to C *)
type func = { func_name : string
            ; func_arg_types : string list
            ; func_result_type : string
            }

(* a vocab is a combination of the above "(" "Vocab" <id> <TYPES-CLAUSE>   
   <PREDS-CLAUSE> <CONSTANTS-CLAUSE> <FUNCTIONS-CLAUSE> ")"                *)
type vocab = { vocab_name : string
             ; types : type_decl list
             ; preds : pred list
             ; consts : const list
             ; funcs : func list
             }


(* Show - functions for turning vocab into strings for displaying and
   debugging *)

let empty_check f vals = match f, vals with
  | f, [] -> ""
  | f, xs -> f xs

let show_type_decl : int -> type_decl -> string =
  fun i t ->
      indent i ^
      "(Type " ^
      t.type_name ^
      empty_check (fun xs -> " > " ^ (unwords t.subtypes))
                  t.subtypes ^
      ")"

let show_type_decls : int -> type_decl list -> string =
  fun i ts ->
      indent i ^
      "(Types\n" ^
      unlines (List.map (show_type_decl (i + 1)) ts) ^
      ")"

let show_pred : int -> pred -> string =
  fun i p ->
      indent i ^
      "(Predicate " ^
      p.pred_name ^
      " " ^
      (unwords p.pred_type) ^
      ")"

let show_preds : int -> pred list -> string =
  fun i ps ->
      indent i ^
      "(Predicates\n" ^
      unlines (List.map (show_pred (i + 1)) ps) ^
      ")"

let maybe_show_preds : int -> pred list -> string =
  fun i ps ->
      empty_check (show_preds i) ps

let show_const : int -> const -> string =
  fun i c ->
    indent i ^
    "(Constant " ^
    c.const_name ^ 
    " " ^ 
    c.const_type ^
    ")"

let show_consts : int -> const list -> string =
  fun i cs ->
      indent i ^
      "(Constants\n" ^
      unlines (List.map (show_const (i + 1)) cs) ^
      ")"

let maybe_show_consts : int -> const list -> string =
  fun i cs ->
    empty_check (show_consts i) cs

let show_func : int -> func -> string =
  fun i f ->
      indent i ^
      "(Function " ^
      f.func_name ^
      " " ^
      (unwords f.func_arg_types) ^
      " " ^
      f.func_result_type ^
      ")"

let show_funcs : int -> func list -> string =
  fun i fs ->
        indent i ^
        "(Functions\n" ^
        unlines (List.map (show_func (i + 1)) fs) ^
        ")"

let maybe_show_funcs : int -> func list -> string =
  fun i fs ->
      empty_check (show_funcs i) fs

(** Produces a prettified string representation of the vocab *)
let show_vocab :  vocab -> string =
  fun v ->
    let t = (show_type_decls 1 v.types) in
    let p = (maybe_show_preds 1 v.preds) in
    let c = (maybe_show_consts 1 v.consts) in
    let f = (maybe_show_funcs 1 v.funcs) in
    "(Vocab " ^
    v.vocab_name ^
    "\n" ^
    t ^
    (if "" = p then "" else "\n" ^ p) ^
    (if "" = c then "" else "\n" ^ c) ^
    (if "" = f then "" else "\n" ^ f) ^
    ")"
  
