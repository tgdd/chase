(** Defines types for the AST of a policy, as well as functions for reading
    a policy from a channel, converting a policy AST to a string, and printing
    or outputting a policy.
    
    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

open StringUtil
open SortedFol

(******************************************************************************)
(* AST TYPES *)
(** Policy free variable declaration. *)
type variable = { variable_name : string
                ; variable_sort : string
                }

(** Policy rule override declartion. *)
type rule_override = { low_rule : string
                     ; high_rules : string list
                     }

(** Policy rule combination declaration. *)
type rule_comb = { rule_fas : string list list
                 ; rule_overrides : rule_override list
                 }

(** Policy rule definition. *)
type rule = { rule_name : string
            ; rule_decision : string
            ; rule_parameters : string list
            ; rule_body : formula
            }

(** Policy definition. *)
type policy = { uses : string
              ; target : formula
              ; vars : variable list
              ; rules : rule list
              ; rule_combs : rule_comb
              }
(* END AST TYPES *)
(******************************************************************************)


(******************************************************************************)
(* SHOW *)
(* variables *)
let show_variable : int -> variable -> string =
  fun i v ->
    indent i ^ 
    "(Variable " ^ 
    v.variable_name ^ 
    " " ^ 
    v.variable_sort ^ 
    ")"

let show_variables : int -> variable list -> string =
  fun i vs ->
      indent i ^
      "(Variables\n" ^
      unlines (List.map (show_variable (i+1)) vs) ^
      ")"

let show_target : int -> formula -> string =
  fun i t ->
      indent i ^
      "(Target " ^
      show_formula t ^
      ")"

(* rule overrides *)
let show_rule_override : int -> rule_override -> string =
  fun i ro ->
      indent i ^
      "(Override " ^
      ro.low_rule ^
      " " ^
      unwords ro.high_rules ^
      ")"
      
let show_rule_fa : int -> string list -> string =
  fun i fas ->
      indent i ^
      "(FA " ^
      unwords fas ^
      ")"

let show_rule_comb : int -> rule_comb -> string =
  fun i rc ->
      indent i ^
      "(RComb\n" ^
      unlines (List.map (show_rule_fa (i+1)) rc.rule_fas) ^
      "\n" ^
      unlines (List.map (show_rule_override (i+1)) rc.rule_overrides) ^
      ")"

(* rules *)
let show_rule : int -> rule -> string =
  fun i r ->
      indent i ^
      "(" ^
      r.rule_name ^
      " = (" ^
      unwords (r.rule_decision::r.rule_parameters) ^
      ") :- " ^
      show_formula r.rule_body ^
      ")"
      
let show_rules : int -> rule list -> string =
  fun i rs ->
      indent i ^
      "(Rules\n" ^
      unlines (List.map (show_rule (i + 1)) rs) ^
      ")"

let show_vars : int -> variable list -> string =
  fun i vs ->
      indent i ^
      "(Variables\n" ^
       unlines (List.map (show_variable (i+1)) vs) ^
      ")"

(* policy *)
let show_policy : policy -> string = 
  fun p ->
      let target = show_target 1 p.target in
      let vars = show_vars 1 p.vars in
      let rules = show_rules 1 p.rules in
      let rule_combs = show_rule_comb 1 p.rule_combs in
      "(Policy uses " ^ p.uses ^
      "\n" ^ target ^
      "\n" ^ vars ^
      "\n" ^ rules ^
      "\n" ^ rule_combs ^ ")"
(* END SHOW *)
(******************************************************************************)
