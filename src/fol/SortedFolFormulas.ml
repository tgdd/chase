(** Module for sorted first order logic type definitions and utilities.

    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)
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

