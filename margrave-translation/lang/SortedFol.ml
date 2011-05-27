open StringUtil 
open ListUtil
open FunUtil

(******************************************************************************)
(* Types *)

(* for ease of reading *)
type var_t = string
type sort_t = string
type pred_t = string
type func_t = string

(* actual data structures *)
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
module StringGraph = Graph.Persistent.Digraph.Concrete(GraphUtil.StringVertex)

(* for ease of reading *)
module SG = StringGraph (* sort graph *)
module PM = StringMap  (* pred map *)
module FM = StringMap (* func map *)

(* Signature *)
module Signature =
  struct
    type t = { sorts : SG.t
             ; funcs : (sort_t list * sort_t) FM.t
             ; preds : sort_t list PM.t
             }
    let empty = { sorts = SG.empty
                ; funcs = FM.empty
                ; preds = PM.empty
                }
    
    let add_sort sgn sup subs =
      let build_graph g sub = SG.add_edge g sup sub in
      { sgn with sorts = List.fold_left build_graph sgn.sorts subs }
    
    let add_func sgn f arity res =
      if FM.mem f sgn.funcs then invalid_arg "func already in sig" else
      { sgn with funcs = FM.add f (arity, res) sgn.funcs }
    
    let add_pred sgn p arity =
      if PM.mem p sgn.preds then invalid_arg "pred already in sig" else
      { sgn with preds = PM.add p arity sgn.preds }
    
    let sort_mem sgn s = SG.mem_vertex sgn.sorts s
    let is_subsort sgn sup sub = SG.mem_edge sgn.sorts sup sub
    let func_mem sgn f = FM.mem f sgn.funcs
    let func_rank sgn f = FM.find f sgn.funcs
    let pred_mem sgn p = PM.mem p sgn.preds
    let pred_arity sgn p = PM.find p sgn.preds
    
    let validate_sort sgn name s =
      if SG.mem_vertex sgn.sorts s then [] else 
      ["Sort " ^ s ^ " is used in a definition of " ^ name ^ ", but does not " ^
       "appear in the sorts of this signature."]
    let validate_func sgn f rank =
      validate_sort sgn f (snd rank) @
      map_append (validate_sort sgn f) (fst rank)
    let validate_pred sgn p arity =
      map_append (validate_sort sgn p) arity
    let validate sgn =
      let msgs = FM.fold (fun f rank msgs -> (validate_func sgn f rank) @ msgs)
                         sgn.funcs [] in
      let msgs = PM.fold (fun p arity msgs -> (validate_pred sgn p arity) @ msgs)
                         sgn.preds msgs in
      msgs
    
    let is_valid sgn = is_empty (validate sgn)
  end

type sig_t = Signature.t

type term = Var of var_t 
          | FunApp of func_t * term list
type formula = And of formula list
             | Or of formula list
             | Not of formula
             | Implies of formula * formula
             | Iff of formula * formula
             | Exists of var_t * sort_t * formula
             | Forall of var_t * sort_t * formula
             | Equals of term * term
             | Pred of pred_t * term list
  
let tru = And([])
let fals = Or([])

type theory = formula list




(******************************************************************************)
(* Simplifying formulas *)

(* helpers for flatten_and_or *)
let is_and f = match f with
  | And(_) -> true
  | _ -> false
let extract_and f = match f with
  | And(fs) -> fs
  | _ -> [f]

let is_or f = match f with
  | Or(_) -> true
  | _ -> false
let extract_or f = match f with
  | Or(fs) -> fs
  | _ -> [f]

(* Flattens out nested And and Or constructs. Useful for printing *)
let rec flatten_and_or : formula -> formula =
  let flatten = flatten_and_or in
  fun f -> match f with
    | And(fs) -> And(map_append extract_and (List.map flatten fs))
    | Or(fs) -> Or(map_append extract_or (List.map flatten fs))
    | Not(f) -> Not(flatten f)
    | Implies(lhs, rhs) -> Implies(flatten lhs, flatten rhs)
    | Iff(lhs, rhs) ->  Iff(flatten lhs, flatten rhs)
    | Exists(name, sort, body) -> Exists(name, sort, flatten body)
    | Forall(name, sort, body) -> Forall(name, sort, flatten body)
    | Equals(_, _) -> f
    | Pred(_, _) -> f

let simplify = flatten_and_or

(******************************************************************************)
(* For displaying formulas and terms *)
let rec show_term : term -> string =
  fun t ->
    match t with
      | Var(name) -> name
      | FunApp(name, []) -> "'" ^ name
      | FunApp(fname, fargs) -> fname ^ 
                                "(" ^ 
                                comma_delim (List.map show_term fargs) ^ 
                                ")" 

let rec show_formula : formula -> string =
  fun f -> match f with
    | And([]) -> "TRUE"
    | And(fs) ->
      "(and " ^ unwords (List.map show_formula fs) ^ ")"
    | Or([]) -> "FALSE"
    | Or(fs) -> 
      "(or " ^ unwords (List.map show_formula fs) ^ ")"
    | Not(body) -> 
      "(not " ^ (show_formula body) ^ ")"
    | Implies(lhs, rhs) -> 
      "(" ^ (show_formula lhs) ^ " -> " ^ (show_formula rhs) ^ ")"
    | Iff(lhs, rhs) -> 
      "(" ^ (show_formula lhs) ^ 
      " iff " ^ (show_formula rhs) ^ ")"
    | Exists(v, vtype, f) -> 
      show_quant "exists" v vtype f
    | Forall(v, vtype, f) -> 
      show_quant "forall" v vtype f
    | Equals(lhs, rhs) -> 
      "(" ^ (show_term lhs) ^ " = " ^ (show_term rhs) ^ ")"
    | Pred(pred, terms) -> 
      "(" ^ unwords (pred :: (List.map show_term terms)) ^ ")"
and show_quant : string -> var_t -> sort_t -> formula -> string =
  fun quant v vtype f ->
      "(" ^ quant ^ " " ^ v ^ " : " ^ vtype ^ " " ^ show_formula f ^ ")"

let output_formulas oc formulas =
  let formulas = List.map flatten_and_or formulas in
  	output_string oc (unlines (List.map show_formula formulas))

(* show for LaTeX *)

let rec latex_term : term -> string =
  fun t -> match t with
    | Var(name) -> name
    | FunApp(name, args) ->
      "\\operatorname{" ^ name ^ "}( " ^
      comma_delim (List.map latex_term args) ^ ")"

let tex_parens = brace_with " \\left( " " \\right) "

let rec latex_formula : formula -> string =
  fun f -> match f with
    | And([]) -> "\\top"
    | And(fs) -> 
      "\\bigwedge\\left\\lbrace " ^ comma_delim (List.map latex_formula fs) ^
      " \\right\\rbrace"
    | Or([]) -> "\\bot"
    | Or(fs) ->
      "\\bigvee\\left\\lbrace " ^ comma_delim (List.map latex_formula fs) ^ 
      " \\right\\rbrace"
    | Not(f) -> tex_parens ("\\neg " ^ latex_formula f)
    | Implies(lhs, rhs) ->
      tex_parens (latex_formula lhs ^ " \\Rightarrow " ^ latex_formula rhs)
    | Iff(lhs, rhs) -> 
      tex_parens (latex_formula lhs ^ " \\Leftrightarrow " ^ latex_formula rhs)
    | Exists(name, sort, f) ->
     latex_quant "\\exists" name sort f
    | Forall(name, sort, f) -> 
     latex_quant "\\forall" name sort f
    | Equals(lhs, rhs) ->
      tex_parens (latex_term lhs ^ " = " ^ latex_term rhs)
    | Pred(name, args) ->
      "\\operatorname{" ^ name ^ "}( " ^
      comma_delim (List.map latex_term args) ^ ")"
and latex_quant : string -> var_t -> sort_t -> formula -> string =
  fun quant name sort f ->
    tex_parens (quant ^ "^{" ^ sort ^ "} " ^ name ^ " . " ^ latex_formula f)

let show_latex_formula = latex_formula

let output_latex_formulas oc formulas =
  let formulas = List.map flatten_and_or formulas in
  	output_string oc & unlines & List.map ((brace_with "\\[" "\\]") <.>
                                          latex_formula)
                                        formulas




(******************************************************************************)
(* Information about formulas *)

(* finding free vars *)
(* helper *)
let not_contains env term =
  if List.mem term env then [] else [term]

let rec term_free_vars env term = match term with 
  | Var(name) -> not_contains env name
  | FunApp(name, terms) -> map_append (term_free_vars env) terms

let rec free_vars' : var_t list -> formula -> var_t list =
  fun env fmla -> match fmla with
    | And(fs) -> map_append (free_vars' env) fs
    | Or(fs) -> map_append (free_vars' env) fs
    | Not(f) -> free_vars' env f
    | Implies(lhs,rhs) -> free_vars' env lhs @ free_vars' env rhs
    | Iff(lhs,rhs) -> free_vars' env lhs @ free_vars' env rhs
    | Exists(v,s,f) -> free_vars' (v::env) f
    | Forall(v,s,f) -> free_vars' (v::env) f
    | Equals(lhs,rhs) -> term_free_vars env lhs @ term_free_vars env rhs
    | Pred(name, terms) -> map_append (term_free_vars env) terms

let free_vars fmla = free_vars' [] fmla

let is_sentence fmla = is_empty (free_vars fmla)

type environment = (var_t * sort_t) list

let rec term_signature_violations : sig_t -> var_t list -> term -> string list =
  fun sgn env t -> match t with
    | Var(name) ->
      if List.mem name env then [] else
      ["The variable " ^ name ^ "was not bound and was not provided in " ^
       "the list of allowed free variables."]
    | FunApp(name, args) ->
      let viols = term_signature_violations sgn env in
      let msgs = map_append viols args in
      if Signature.func_mem sgn name then msgs else
      ("Function name " ^ name ^ " does not appear in the signature.") :: msgs

let rec signature_violations : sig_t
                            -> var_t list
                            -> formula
                            -> string list =
  fun sgn env fmla -> match fmla with
    | And(fs) -> map_append (signature_violations sgn env) fs
    | Or(fs) -> map_append (signature_violations sgn env) fs
    | Not(f) -> signature_violations sgn env f
    | Implies(lhs,rhs) ->
      let viols = signature_violations sgn env in
      viols lhs @ viols rhs
    | Iff(lhs,rhs) ->
      let viols = signature_violations sgn env in
      viols lhs @ viols rhs
    | Exists(v,s,f) ->
      let msgs = signature_violations sgn (v::env) f in
      if Signature.sort_mem sgn s then msgs else
      ("The sort name " ^ s ^ " does not appear in the signature.") :: msgs
    | Forall(v,s,f) ->
      let msgs = signature_violations sgn (v::env) f in
      if Signature.sort_mem sgn s then msgs else
      ("The sort name " ^ s ^ " does not appear in the signature.") :: msgs
    | Equals(lhs,rhs) ->
      let viols = term_signature_violations sgn env in
      viols lhs @ viols rhs
    | Pred(name, terms) ->
      let msgs = map_append (term_signature_violations sgn env) terms in
      if Signature.pred_mem sgn name then msgs else
      ("Predicate name " ^ name ^ " does not appear in the signature.") :: msgs

let meets_signature sgn env fmla = is_empty (signature_violations sgn env fmla)

let rec is_subsort sgn sub sup = match sub, sup with
    | [], [] -> true
    | (x::xs), (y::ys) when Signature.is_subsort sgn x y ->
      is_subsort sgn xs ys
    | _ -> false

let rec term_sort_violations : sig_t -> environment -> term -> (string list * sort_t) =
  fun sgn env t -> match t with
    | Var(name) -> ([], List.assoc name env)
    | FunApp(name, args) -> 
      let res = List.map (term_sort_violations sgn env) args in
      let msgs = map_append fst res in
      let used_arity = List.map snd res in
      let expected_arity, ret_sort = Signature.func_rank sgn name in
      if is_subsort sgn expected_arity used_arity then (msgs, ret_sort)  else
      (("Function " ^ name ^ " expects arity (" ^ comma_delim expected_arity ^
       ") but was used with arity (" ^ comma_delim used_arity ^ ").") :: msgs,
       ret_sort)

let rec sort_violations : sig_t -> environment -> formula -> string list =
  fun sgn env fmla -> match fmla with
    | And(fs) -> map_append (sort_violations sgn env) fs
    | Or(fs) -> map_append (sort_violations sgn env) fs
    | Not(f) -> sort_violations sgn env f
    | Implies(lhs,rhs) ->
      let viols = sort_violations sgn env in
      viols lhs @ viols rhs
    | Iff(lhs,rhs) ->
      let viols = sort_violations sgn env in
      viols lhs @ viols rhs
    | Exists(v,s,f) -> sort_violations sgn ((v,s)::env) f
    | Forall(v,s,f) -> sort_violations sgn ((v,s)::env) f
    | Equals(lhs,rhs) ->
      (* todo: check for connected components *)
      let viols = term_sort_violations sgn env in
      let lhs_msgs, lhs_sort = viols lhs in
      let rhs_msgs, rhs_sort = viols rhs in
      lhs_msgs @ rhs_msgs
    | Pred(name, args) ->
      let res = List.map (term_sort_violations sgn env) args in
      let msgs = map_append fst res in
      let used_arity = List.map snd res in
      let expected_arity = Signature.pred_arity sgn name in
      if is_subsort sgn expected_arity used_arity then msgs else
      ("Predicate " ^ name ^ " expects arity (" ^ comma_delim expected_arity ^
       ") but was used with arity (" ^ comma_delim used_arity ^ ").") :: msgs


let well_sorted sgn env fmla = is_empty (sort_violations sgn env fmla)

let well_formed  : sig_t -> environment -> formula -> bool =
  fun sgn env fmla ->
    meets_signature sgn (List.map fst env) fmla || well_sorted sgn env fmla