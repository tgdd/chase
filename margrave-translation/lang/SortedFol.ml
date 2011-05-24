open StringUtil 

type term = Var of string
          | Const of string
          | FunApp of string * term list
type formula = And of formula list
             | Or of formula list
             | Not of formula
             | Implies of formula * formula
             | Iff of formula * formula
             | Exists of string * string * formula
             | Forall of string * string * formula
             | Equals of term * term
             | Pred of string * term list
             | True
             | False
  
type theory = formula list

let rec s2u_t : term -> Fol.term =
  fun t -> match t with
    | Var(v) -> Fol.Var(v)
    | Const(c) -> Fol.Const(c)
    | FunApp(f, args) -> Fol.FunApp(f, List.map s2u_t args)
let sorted2unsorted_term = s2u_t

let rec s2u : formula -> Fol.formula =
  fun fmla -> match fmla with
    | And(fmlas) -> Fol.And(List.map s2u fmlas)
    | Or(fmlas) -> Fol.Or(List.map s2u fmlas)
    | Not(fmla) -> Fol.Not(s2u fmla)
    | Implies(lhs, rhs) -> Fol.Implies(s2u lhs, s2u rhs)
    | Iff(lhs, rhs) -> Fol.Iff(s2u lhs, s2u rhs)
    | Exists(v,t,body) -> Fol.Exists(v, Fol.And([ Fol.Pred(t,[Fol.Var(v)])
                                                ; s2u body ]))
    | Forall(v,t,body) -> Fol.Forall(v, Fol.Implies(Fol.Pred(t,[Fol.Var(v)]),
                                                    s2u body))
    | Equals(lhs,rhs) -> Fol.Equals(s2u_t lhs, s2u_t rhs)
    | Pred(p,args) -> Fol.Pred(p, List.map s2u_t args)
    | False -> Fol.False
    | True -> Fol.True
let sorted2unsorted = s2u


(* Show *)
let rec show_term : term -> string =
  fun t ->
    match t with
      | Var(name) -> name
      | Const(name) -> "'" ^ name
      | FunApp(fname, fargs) -> fname ^ 
                                "(" ^ 
                                String.concat "," (List.map show_term fargs) ^ 
                                ")" 

let rec show_formula : formula -> string =
  fun f ->
      match f with
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
        | Exists(v, vtype, f) -> 
          show_quant "exists" v vtype f
        | Forall(v, vtype, f) -> 
          show_quant "forall" v vtype f
        | Equals(lhs, rhs) -> 
          "(" ^ (show_term lhs) ^ " = " ^ (show_term rhs) ^ ")"
        | Pred(pred, terms) -> 
          "(" ^ unwords (pred :: (List.map show_term terms)) ^ ")"
        | True -> "TRUE"
        | False -> "FALSE"
and show_quant =
  fun quant v vtype f ->
      "(" ^ quant ^ " " ^ v ^ " : " ^ vtype ^ " " ^ show_formula f ^ ")"
