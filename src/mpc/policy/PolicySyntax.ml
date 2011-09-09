(** Defines types for the AST of a policy, as well as functions for reading
    a policy from a channel, converting a policy AST to a string, and printing
    or outputting a policy.
    
    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

open StringUtil
open SortedFol.Syntax

(******************************************************************************)
(* AST TYPES *)
(** Policy free variable declaration. *)
type variable = { variable_name : string
                ; variable_sort : string
                }

(** Policy decision conflict resolution declaration. *)
type dcrd = FirstApplicable of string list
          | Overrides of string * (string list)

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
              ; dcrds : dcrd list
              }
(* END AST TYPES *)
(******************************************************************************)


(******************************************************************************)
(* SHOW *)
(* variables *)
let show_variable : int -> variable -> string =
  fun i v ->
    ( indent i
    ^ "(Variable "
    ^ v.variable_name
    ^ " "
    ^ v.variable_sort
    ^ ")"
    )

let show_variables : int -> variable list -> string =
  fun i vs ->
    ( indent i
    ^ "(Variables\n"
    ^ unlines (List.map (show_variable (i+1)) vs)
    ^ ")"
    )

let show_target : int -> formula -> string =
  fun i t ->
    ( indent i
    ^ "(Target "
    ^ show_formula t
    ^ ")"
    )

(* rule overrides *)
let show_rule_override : int -> string -> string list -> string =
  fun i low highs ->
    ( indent i
    ^ "(Override "
    ^ low
    ^ " "
    ^ unwords highs
    ^ ")"
    )
      
let show_rule_fa : int -> string list -> string =
  fun i ds ->
    ( indent i
    ^ "(FA "
    ^ unwords ds
    ^ ")"
    )

let show_dcrd : int -> dcrd -> string =
  fun i dcrd ->
    match dcrd with
    | FirstApplicable ds -> show_rule_fa i ds
    | Overrides (low, highs) -> show_rule_override i low highs
  

let show_dcrds : int -> dcrd list -> string =
  fun i dcrds ->
      ( indent i
      ^ "(RComb\n"
      ^ unlines (List.map (show_dcrd (i+1)) dcrds)
      ^ ")"
      )

(* rules *)
let show_rule : int -> rule -> string =
  fun i r ->
    ( indent i
    ^ "("
    ^ r.rule_name
    ^ " = ("
    ^ unwords (r.rule_decision::r.rule_parameters)
    ^ ") :- "
    ^ show_formula r.rule_body
    ^ ")"
    )
      
let show_rules : int -> rule list -> string =
  fun i rs ->
    ( indent i
    ^ "(Rules\n"
    ^ unlines (List.map (show_rule (i + 1)) rs)
    ^ ")"
    )

let show_vars : int -> variable list -> string =
  fun i vs ->
    ( indent i
    ^ "(Variables\n"
    ^ unlines (List.map (show_variable (i+1)) vs)
    ^ ")"
    )

(* policy *)
let show_policy : policy -> string = 
  fun p ->
    let target = show_target 1 p.target in
    let vars = show_vars 1 p.vars in
    let rules = show_rules 1 p.rules in
    let dcrds = show_dcrds 1 p.dcrds in
    ( "(Policy uses " ^ p.uses
    ^ "\n" ^ target
    ^ "\n" ^ vars
    ^ "\n" ^ rules
    ^ "\n" ^ dcrds ^ ")"
    )
(* END SHOW *)
(******************************************************************************)
