(** Defines types for the AST of a policy, as well as functions for reading
    a policy from a channel, converting a policy AST to a string, and printing
    or outputting a policy.
    
    @author Theophilos Giannakopoulos (tgiannak@alum.wpi.edu) *)

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
            ; rule_body : SortedFol.formula
            }
              
(** Policy definition. *)
type policy = { uses : string
              ; target : SortedFol.formula
              ; vars : variable list
              ; rules : rule list
              ; rule_combs : rule_comb
              }
(* END AST TYPES *)
(******************************************************************************)


(******************************************************************************)
(* SHOW *)
(** Creates a string representation of a policy. *)
val show_policy : policy -> string
(* END SHOW *)
(******************************************************************************)
