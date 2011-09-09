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

(** Policy decision conflict resolution declaration. *)
type dcrd = FirstApplicable of string list
          | Overrides of string * (string list)

(** Policy rule definition. *)
type rule = { rule_name : string
            ; rule_decision : string
            ; rule_parameters : string list
            ; rule_body : SortedFol.Syntax.formula
            }
              
(** Policy definition. *)
type policy = { uses : string
              ; target : SortedFol.Syntax.formula
              ; vars : variable list
              ; rules : rule list
              ; dcrds : dcrd list
              }
(* END AST TYPES *)
(******************************************************************************)


(******************************************************************************)
(* SHOW *)
(** Creates a string representation of a policy. *)
val show_policy : policy -> string
(* END SHOW *)
(******************************************************************************)
