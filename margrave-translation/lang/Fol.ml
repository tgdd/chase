open StringUtil 

type term = Var of string
          | FunApp of string * term list
type formula = And of formula list
             | Or of formula list
             | Not of formula
             | Implies of formula * formula
             | Iff of formula * formula
             | Exists of string * formula
             | Forall of string * formula
             | Equals of term * term
             | Pred of string * term list
  
type theory = formula list

(* Show *)
let rec show_term : term -> string =
  fun t ->
    match t with
      | Var(name) -> name
      | FunApp(name, []) -> "'" ^ name
      | FunApp(fname, fargs) -> fname ^
                                "(" ^
                                String.concat "," (List.map show_term fargs) ^
                                ")" 

let rec show_formula : formula -> string =
  fun f ->
      match f with
        | And([]) -> "TRUE"
        | Or([]) -> "FALSE"
        | And(fs) ->
          "(and " ^ unwords (List.map show_formula fs) ^ ")"
        | Or(fs) -> 
          "(or " ^ unwords (List.map show_formula fs) ^ ")"
        | Not(body) -> 
          "(not " ^ (show_formula body) ^ ")"
        | Implies(lhs, rhs) -> 
          "(" ^ (show_formula lhs) ^ " -> " ^ (show_formula rhs) ^ ")"
        | Iff(lhs, rhs) -> 
          "(" ^ (show_formula lhs) ^ 
          " iff " ^ (show_formula rhs) ^ ")"
        | Exists(v, f) -> 
          show_quant "exists" v f
        | Forall(v, f) -> 
          show_quant "forall" v f
        | Equals(lhs, rhs) -> 
          "(" ^ (show_term lhs) ^ " = " ^ (show_term rhs) ^ ")"
        | Pred(pred, terms) -> 
          "(" ^ unwords (pred :: (List.map show_term terms)) ^ ")"
and show_quant =
  fun quant v f ->
      "(" ^ quant ^ " " ^ v ^ " " ^ show_formula f ^ ")"
