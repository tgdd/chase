open StringUtil 
open ListUtil
open FunUtil

(* little helpers *)
let (<::?>) mx xs = match mx with
  | None -> xs
  | Some x -> x::xs

(******************************************************************************)
(* Types *)

(* for ease of reading *)
type var_t = string
type sort_t = string
type pred_t = string
type func_t = string

type signature =
  { sort_names : sort_t list
  ; subsort : sort_t -> sort_t -> bool
  ; intersect : sort_t -> sort_t -> bool
  ; func_names : func_t list
  ; func_rank : func_t -> sort_t list * sort_t
  ; pred_names : pred_t list
  ; pred_rank : pred_t -> sort_t list
  ; free_var_names : var_t list
  ; free_var_sort : var_t -> sort_t
  }

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

(* is_sentence. Not just using free vars, because we can be faster than that *)
let rec term_is_sentence env term = match term with 
  | Var(name) -> List.mem name env
  | FunApp(name, terms) -> ListUtil.and_map (term_is_sentence env) terms

let rec is_sentence' env fmla = match fmla with
  | And(fs) -> ListUtil.and_map (is_sentence' env) fs
  | Or(fs) -> ListUtil.and_map (is_sentence' env) fs
  | Not(f) -> is_sentence' env f
  | Implies(lhs,rhs) -> is_sentence' env lhs && is_sentence' env rhs
  | Iff(lhs,rhs) -> is_sentence' env lhs && is_sentence' env rhs
  | Exists(v,s,f) -> is_sentence' (v::env) f
  | Forall(v,s,f) -> is_sentence' (v::env) f
  | Equals(lhs,rhs) -> term_is_sentence env lhs && term_is_sentence env rhs
  | Pred(name, terms) -> ListUtil.and_map (term_is_sentence env) terms


let is_sentence = is_sentence' []

type environment = (var_t * sort_t) list

let get_type : environment -> signature -> term -> sort_t =
  fun env sgn term -> match term with
    | Var(name) -> 
      (try List.assoc name env
       with Not_found -> sgn.free_var_sort name)
    | FunApp(name, args) -> snd (sgn.func_rank name)

let well_formed_term env sgn term = []

(** An Equals is well formed if each of the LHS and RHS is well-formed and the
    LHS and RHS have intersecting types. *)
let well_formed_equals : environment -> signature -> term -> term -> string list =
  fun env sgn lhs rhs ->
    let msgs = (well_formed_term env sgn lhs @ well_formed_term env sgn rhs) in
    if not (is_empty msgs) then msgs else
    let lhs_type = get_type env sgn lhs in
    let rhs_type = get_type env sgn rhs in
    (if sgn.intersect lhs_type rhs_type
    then None
    else Some ("The types " ^ lhs_type ^ " and " ^ rhs_type ^ " do not intersect. " ^
               show_formula (Equals(lhs, rhs)) ^ "will always be false.")) <::?>
    msgs

(** A Pred is well formed if the name is in the signature, the types of the
    arguments are well formed and the arguments match the types declared in
    the signature. *)
let well_formed_pred : environment
                    -> signature
                    -> pred_t
                    -> term list
                    -> string list =
  fun env sgn name terms ->
    let msgs = map_append (well_formed_term env sgn) terms in
    if not (is_empty msgs) then msgs else
    let types = List.map (get_type env sgn) terms in
    (if List.mem name sgn.pred_names 
     then (if sgn.pred_rank name = types then None
           else Some ("The type of predicate " ^ name ^
                      " does not match the signature."))
     else Some ("The predicate name " ^ name ^
                " does not appear in the signature.")) <::?>
    msgs

let rec well_formed' : environment
                    -> signature
                    -> formula
                    -> string list =
  fun env sgn fmla -> match fmla with
    | And(fs) -> ListUtil.map_append (well_formed' env sgn) fs
    | Or(fs) -> ListUtil.map_append (well_formed' env sgn) fs
    | Not(f) -> well_formed' env sgn f
    | Implies(lhs,rhs) -> well_formed' env sgn lhs @ well_formed' env sgn rhs
    | Iff(lhs,rhs) -> well_formed' env sgn lhs @ well_formed' env sgn rhs
    | Exists(v,s,f) -> well_formed_quant env sgn v s f
    | Forall(v,s,f) -> well_formed_quant env sgn v s f
    | Equals(lhs,rhs) -> well_formed_equals env sgn lhs rhs
    | Pred(name, terms) -> well_formed_pred env sgn name terms

and well_formed_quant : environment -> signature -> var_t -> sort_t -> formula -> string list =
  fun env sgn v s f ->
    (if (List.mem s sgn.sort_names) then None else
     Some ("The sort " ^ s ^ " does not appear in the signature.")) <::?>
    well_formed' ((v,s)::env) sgn f

let well_formed sgn fmla = 
  let env = (List.map (fun v -> (v, sgn.free_var_sort v)) sgn.free_var_names) in
  well_formed' env sgn fmla